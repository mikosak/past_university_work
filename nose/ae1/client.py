import socket
import sys
import os

# No shared module as these scripts are held in seperate directories

valid_commands = ["put", "get", "list"]
no_error = True

# Used for generating the required report
def report(ip, request, filename, status):

    if request == "get":
        request = "'Get' request was:"
    if request == "put":
        request = "'Put' request was:"
    if request == "list":
        request = "'List' request was"
        
    if status == True:
        status = "SUCCESSFUL."
    else:
        status = "UNSUCCESSFUL."

    print(f"{ip[0]} {ip[1]}", end=" ")
    print(filename, end=" ")
    print(request, end=" ")
    print(status)
    return

# Arguement handling
srv_address = str(sys.argv[1])
try:
    srv_port = int(sys.argv[2])
except:
    print("Error. Invalid port number.")
    quit()
given_request = (str(sys.argv[3])).lower()
if given_request not in valid_commands:
    print("Invalid command. Please try again.")
    quit()
if (given_request == "put") or (given_request == "get"):
    filename = str(sys.argv[4])
else: 
    filename = "LISTING"
    
# Connection setup
cli_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
try:
    cli_sock.connect((srv_address, srv_port))
except ConnectionRefusedError:
    print("Target unreachable. Please try again.")
    quit()
except Exception as e:
    print(e)
    quit()
print("Connected to server")
    
# Request processing
try:    
    # Put requests
    if given_request == "put":
        cli_sock.sendall(("p," + filename).encode())
        with open(filename, "rb") as f:
            while True:
                bin_data = f.read(4096)
                if not bin_data:
                    break
                cli_sock.sendall(bin_data)
    
    # Get requests
    if given_request == "get":
        cli_sock.sendall(("g," + filename).encode())
        with open(filename, "xb") as f:
            while True:
                bin_data = cli_sock.recv(4096)
                if not bin_data:
                    break
                f.write(bin_data)
    
    # List requests
    if given_request == "list":
        cli_sock.sendall(("l," + filename).encode())
        list_data = ""
        while True:
            recieved = cli_sock.recv(4096)
            if not recieved:
                break
            list_data += recieved.decode()
            
        listing = (list_data).split(",")
        print("")
        print("The files in the current directory are:")
        for i in listing:
            print(i)
    
# Error handling
except FileExistsError:
    print("This file already exists and cannot be overwritten.")
    no_error = False
except FileNotFoundError:
    print("This file could not be found. Please try again.")
    no_error = False
except Exception as e:
    print(e)
    no_error = False
else:    
    no_error = True
finally:
    report((srv_address, srv_port), given_request, filename, no_error)
    cli_sock.close()
    