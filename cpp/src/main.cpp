#include "SRS.h"
#include "../include/PracticalSocket.h"
#include <pthread.h>

using namespace std;
using namespace srs;

#define ALPHA 4
#define BASE_MAX 10000.0

const int RCVBUFSIZE = 32;
void handleRequest(TCPSocket *sock);     // TCP client handling function
void *ThreadMain(void *arg);             // Main program of a thread

// RecSys and its mutex
// RecSys in a singleton
static SRS& getSRS() {
    static SRS recSys(ALPHA, BASE_MAX);
    return recSys;
}
pthread_mutex_t    mutex = PTHREAD_MUTEX_INITIALIZER;

int main(int argc, char **argv) {
    if (argc != 2) {
        cerr << "Usage: " << argv[0] << " <Server Port> " << endl;
        return EXIT_FAILURE;
    }

    unsigned short port = atoi(argv[1]);
    TCPServerSocket servSock(port);  // Socket descriptor for server

    setlocale(LC_ALL, "en_US.utf8"); // Set locale (for wcstring)

    try {
        PlEngine engine(argc, argv); // Init Prolog engine
    } catch(PlError &e) {
        cerr << e.message;
        return EXIT_FAILURE;
    }

    try {
        for (;;) {
            // Create separate memory for client argument  
            TCPSocket *clntSock = servSock.accept();

            // Create client thread  
            pthread_t threadID;
            if(pthread_create(&threadID, NULL, ThreadMain, (void *) clntSock) != 0) {
                cerr << "Unable to create thread" << endl;
                return EXIT_FAILURE;
            }
        }
    } catch (SocketException &e) {
        cerr << e.what() << endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

// TCP client handling function
void handleRequest(TCPSocket *sock) {
    char buffer[RCVBUFSIZE];
    memset(buffer, '\0', RCVBUFSIZE);
    int recvMsgSize;

    while((recvMsgSize = sock->recv(buffer, RCVBUFSIZE)) > 0) {
        cout << "[!] Received command: " << buffer << "\n";
        if(!strcmp(buffer, "UPDATE")) {
            cout << "test\n";
            pthread_mutex_lock(&mutex);
            getSRS().updateDB();
            cout << "[+] Database updated..\n";
            getSRS().generatePlans();
            cout << "[+] Generated plans..\n";
            pthread_mutex_unlock(&mutex);
            sock->send("OK", 2);
        } else if(!strcmp(buffer, "RECOMMENDATION")) {
            long user = 0;
            if(sock->recv(buffer, RCVBUFSIZE) > 0 && (user = atol(buffer))) {
                pthread_mutex_lock(&mutex);
                SRS::users users = getSRS().getRecommendation(user);
                pthread_mutex_unlock(&mutex);
                for(auto const rec : users) {
                    sock->send((const void *)rec, sizeof rec);
                    sock->send("\n",1);
                }
                sock->send("OK", 2);
            }
        }
        else {
            sock->send("ERROR",5);
            return;
        }

    }
    // Destructor closes socket
}

void *ThreadMain(void *clntSock) {
    // Guarantees that thread resources are deallocated upon return  
    pthread_detach(pthread_self()); 

    // Extract socket file descriptor from argument  
    handleRequest((TCPSocket *) clntSock);

    delete (TCPSocket *) clntSock;
    return NULL;
}
