/* mem.h
 * 
 * Malloc replacement
 * 
 * Based on chapter 5 in book "C Interfaces and Implementations"
 * by David R. Hanson"
 */
#ifndef _MEM_H_
#define _MEM_H_

#include <stdlib.h>

extern int log_allocation;

void *mem_alloc_location(size_t nbytes, const char *file, int line);
void mem_free_location(void *ptr, const char *file, int line);
char *str_dupl_location(const char *s, const char *file, int line);
void mem_heap_end_address(char *leadString);

/* The following macros automatically insert the file and line. USE THEM! */

#define mem_alloc(nbytes) \
	mem_alloc_location((nbytes), __FILE__, __LINE__)

#define mem_free(ptr) \
	((void)(mem_free_location((ptr), __FILE__, __LINE__)))

#define str_dupl(s) \
	str_dupl_location((s), __FILE__, __LINE__)

#endif /* _MEM_H_ */
