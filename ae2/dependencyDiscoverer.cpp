#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <list>
#include <mutex>
#include <thread>
#include <optional>

class ThreadSafeTable {
  private:
    std::unordered_map<std::string, std::list<std::string>> theTable;
    std::mutex padlock;

  public:

    void insert(const std::string& key, std::list<std::string>& value) {
      std::lock_guard<std::mutex> lock(padlock);
        if (theTable.find(key) == theTable.end()) {
          theTable.insert({key, value});
        } else {
          theTable[key].insert(theTable[key].end(), value.begin(), value.end());
        }
    }

    std::optional<std::list<std::string>> find(const std::string& key) {
      std::lock_guard<std::mutex> lock(padlock);
      auto result = theTable.find(key);
        if (result != theTable.end()) {
          return result->second;
        } else {
          return std::nullopt;
      }
    }

    std::size_t size() {
      return theTable.size();
    }
};

class ThreadSafeQ {
  private:
    std::list<std::string> workQ;
    std::mutex padlock;

  public:

    void push_back(const std::string& value) {
      std::lock_guard<std::mutex> lock(padlock);
      workQ.push_back(value);
    }

    void getSizePopAndFrontByReference(int& size_, std::string& front_) {
      std::lock_guard<std::mutex> lock(padlock);
      size_ = workQ.size();
      if (size_ > 0) {
        front_ = workQ.front();
        workQ.pop_front();
      }
    }
};

ThreadSafeTable theTable;
ThreadSafeQ workQ;
std::vector<std::string> dirs;

std::string dirName(const char * c_str) {
  std::string s = c_str; // s takes ownership of the string content by allocating memory for it
  if (s.back() != '/') { s += '/'; }
  return s;
}

std::pair<std::string, std::string> parseFile(const char* c_file) {
  std::string file = c_file;
  std::string::size_type pos = file.rfind('.');
  if (pos == std::string::npos) {
    return {file, ""};
  } else {
    return {file.substr(0, pos), file.substr(pos + 1)};
  }
}

// open file using the directory search path constructed in main()
static FILE *openFile(const char *file) {
  FILE *fd;
  for (unsigned int i = 0; i < dirs.size(); i++) {
    std::string path = dirs[i] + file;
    fd = fopen(path.c_str(), "r");
    if (fd != NULL)
      return fd; // return the first file that successfully opens
  }
  return NULL;
}

// process file, looking for #include "foo.h" lines
static void process(const char *file, std::string filename) {
  char buf[4096], name[4096];
  // 1. open the file
  FILE *fd = openFile(file);
  if (fd == NULL) {
    fprintf(stderr, "Error opening %s\n", file);
    exit(-1);
  }
  while (fgets(buf, sizeof(buf), fd) != NULL) {
    char *p = buf;
    // 2a. skip leading whitespace
    while (isspace((int)*p)) { p++; }
    // 2b. if match #include 
    if (strncmp(p, "#include", 8) != 0) { continue; }
    p += 8; // point to first character past #include
    // 2bi. skip leading whitespace
    while (isspace((int)*p)) { p++; }
    if (*p != '"') { continue; }
    // 2bii. next character is a "
    p++; // skip "
    // 2bii. collect remaining characters of file name
    char *q = name;
    while (*p != '\0') {
      if (*p == '"') { break; }
      *q++ = *p++;
    }
    *q = '\0';
    // 2bii. append file name to dependency list
    std::list<std::string> nameList;
    nameList.push_back(name);
    theTable.insert(filename, nameList);
    // 2bii. if file name not already in table ...
    if (theTable.find(name).has_value()) {continue;}
    // ... insert mapping from file name to empty list in table ...
    std::list<std::string> emptyList;
    theTable.insert(name, emptyList);
    // ... append file name to workQ
    workQ.push_back(name);
  }
  // 3. close file
  fclose(fd);
}

// iteratively print dependencies
static void printDependencies(std::unordered_set<std::string> *printed,
                              std::list<std::string> *toProcess,
                              FILE *fd) {
  if (!printed || !toProcess || !fd) return;

  // 1. while there is still a file in the toProcess list
  while ( toProcess->size() > 0 ) {
    // 2. fetch next file to process
    std::string name = toProcess->front();
    toProcess->pop_front();
    // 3. lookup file in the table, yielding list of dependencies
    auto tempOptional = theTable.find(name);
    std::list<std::string>* ll;

    if (tempOptional.has_value()) {
      ll = &tempOptional.value();
    } else {
      exit(-1);
    }
    // 4. iterate over dependencies
    for (auto iter = ll->begin(); iter != ll->end(); iter++) {
      // 4a. if filename is already in the printed table, continue
      if (printed->find(*iter) != printed->end()) { continue; }
      // 4b. print filename
      fprintf(fd, " %s", iter->c_str());
      // 4c. insert into printed
      printed->insert( *iter );
      // 4d. append to toProcess
      toProcess->push_back( *iter );
    }
  }
}

void worker(ThreadSafeTable& theTable, ThreadSafeQ& workQ) {
  int size_;
  std::string filename;
  while (1) {
    workQ.getSizePopAndFrontByReference(size_, filename);
    if (size_ == 0) {
        break;
      }
    if (theTable.find(filename).has_value()) {
      process(filename.c_str(), filename);
    } else {
      fprintf(stderr, "Mismatch between table and workQ\n");
      exit(-1);
      }
    }
  }

int main(int argc, char *argv[]) {
  // 1. look up CPATH in environment
  char *cpath = getenv("CPATH");
  setenv("CRAWLER_THREADS", "2", 0);
  int workerCount = std::stoi(getenv("CRAWLER_THREADS"));

  // determine the number of -Idir arguments
  int i;
  for (i = 1; i < argc; i++) {
    if (strncmp(argv[i], "-I", 2) != 0)
      break;
  }
  int start = i;

  // 2. start assembling dirs vector
  dirs.push_back( dirName("./") ); // always search current directory first
  for (i = 1; i < start; i++) {
    dirs.push_back( dirName(argv[i] + 2 /* skip -I */) );
  }
  if (cpath != NULL) {
    std::string str( cpath );
    std::string::size_type last = 0;
    std::string::size_type next = 0;
    while((next = str.find(":", last)) != std::string::npos) {
      dirs.push_back( str.substr(last, next-last) );
      last = next + 1;
    }
    dirs.push_back( str.substr(last) );
  }
  // 2. finished assembling dirs vector

  // 3. for each file argument ...
  for (i = start; i < argc; i++) {
    std::pair<std::string, std::string> pair = parseFile(argv[i]);
    if (pair.second != "c" && pair.second != "y" && pair.second != "l") {
      fprintf(stderr, "Illegal extension: %s - must be .c, .y or .l\n",
              pair.second.c_str());
      return -1;
    }

    std::string obj = pair.first + ".o";

    // 3a. insert mapping from file.o to file.ext
    std::list<std::string> argList;
    argList.push_back(argv[i]);
    theTable.insert(obj, argList);
    
    // 3b. insert mapping from file.ext to empty list
    std::list<std::string> emptyList;
    theTable.insert(argv[i], emptyList);
    
    // 3c. append file.ext on workQ
    workQ.push_back(argv[i]);
  }
  
  std::vector<std::thread> threads;
  for (int i = 0; i < workerCount; i++) {
    threads.push_back(std::thread([](){worker(theTable, workQ);}));
  }
  for (auto& thread : threads) {
    thread.join();
  }

  // 5. for each file argument
  for (i = start; i < argc; i++) {
    // 5a. create hash table in which to track file names already printed
    std::unordered_set<std::string> printed;
    // 5b. create list to track dependencies yet to print
    std::list<std::string> toProcess;

    std::pair<std::string, std::string> pair = parseFile(argv[i]);

    std::string obj = pair.first + ".o";
    // 5c. print "foo.o:" ...
    printf("%s:", obj.c_str());
    // 5c. ... insert "foo.o" into hash table and append to list
    printed.insert( obj );
    toProcess.push_back( obj );
    // 5d. invoke
    printDependencies(&printed, &toProcess, stdout);

    printf("\n");
  }

  return 0;
}
