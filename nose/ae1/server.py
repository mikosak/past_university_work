import socket
import sys
import os

# No shared module as these scripts are held in seperate directories

no_error = True

# Used for generating the required report
def report(ip, request, filename, status):

    if request == "g":
        request = "'Get' request was:"
    if request == "p":
        request = "'Put' request was:"
    if request == "l":
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
try:
    given_port = int(sys.argv[1])
except:
    print("Error. Invalid port number.")
    quit()

# Set up socket
srv_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
try:
    srv_sock.bind(("", given_port))
except Exception:
    print("Error setting up socket. Please check that this port is not busy.")
    quit()
print("Server up and running.")
srv_sock.listen(5)

# Request handling loop
while True:
    
    try:
        # Set up connection with client
        cli_sock, cli_addr = srv_sock.accept()
        print("Connection with " + str(cli_addr) + " set up successfully")
        
        # Recieve request code from client
        request = cli_sock.recv(4096).decode()
        request = request.split(",")
        request, filename = request
        
        # Put requests
        if request == "p":
            with open(filename, "xb") as f:
                while True:
                    bin_data = cli_sock.recv(4096)
                    if not bin_data:
                        break
                    f.write(bin_data)
        
        # Get requests
        if request == "g":
            with open(filename, "rb") as f:
                while True:
                    bin_data = f.read(4096)
                    if not bin_data:
                        break
                    cli_sock.sendall(bin_data)
            
        # List requests
        if request == "l":
            listing = ",".join(os.listdir())
            cli_sock.sendall(listing.encode())
        
        
    # Error handling    
    except FileExistsError as e:
        print("This file already exists and cannot be overwritten.")
        no_error = False
    except FileNotFoundError as e:
        print("This file could not be found. Please try again.") 
        no_error = False
    except Exception as e:
        print(e)
        no_error = False
    else:
        no_error = True
    finally:
        report(cli_addr, request, filename, no_error)
        cli_sock.close()
srv_sock.close()




