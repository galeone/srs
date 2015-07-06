#include "SRS.h"
#include "../include/PracticalSocket.h"
#include <algorithm> // std::find_if_not

using namespace std;
using namespace srs;

#define ALPHA 4
#define BASE_MAX 10000.0

const int RCVBUFSIZE = 32;
void handleRequest(TCPSocket *sock);     // TCP client handling function

// RecSys in a singleton
static SRS& getSRS() {
    static SRS recSys(ALPHA, BASE_MAX);
    return recSys;
}


// No comment for the lack of string::trim
// Thanks to: https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring/17976541#17976541
inline std::string trim(const std::string &s)
{
    auto wsfront = std::find_if_not(s.begin(),s.end(),[](int c){return std::isspace(c);});
    return std::string(wsfront,std::find_if_not(s.rbegin(),std::string::const_reverse_iterator(wsfront),[](int c){return std::isspace(c);}).base());
}

int main(int argc, char **argv) {
    if (argc != 2) {
        cerr << "Usage: " << argv[0] << " <Server Port> " << endl;
        return EXIT_FAILURE;
    }

    unsigned short port = atoi(argv[1]);
    TCPServerSocket servSock(port);  // Socket descriptor for server

    try {
        setlocale(LC_ALL, "en_US.utf8"); // Set locale (for wcstring)
        PlEngine engine(argc, argv); // Init Prolog engine
        for (;;) {
            handleRequest(servSock.accept());
        }
    } catch(PlError &e) {
        cerr << e.message;
        return EXIT_FAILURE;
    } catch (SocketException &e) {
        cerr << e.what() << endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

#define ERROR() sock->send("ERROR\n",6)
#define OK() sock->send("OK\n",3)

// TCP client handling function
void handleRequest(TCPSocket *sock) {
    char buffer[RCVBUFSIZE];
    memset(buffer, '\0', RCVBUFSIZE);

    while(sock->recv(buffer, RCVBUFSIZE) > 0) {
        cout << "[!] Received command: " << buffer << "\n";
        string command(buffer);
        command = trim(command);
        if(command == "UPDATE") {
            sock->send("WAIT\n",5);
            getSRS().updateDB();
            cout << "[+] Database updated..\n";
            getSRS().generatePlans();
            cout << "[+] Generated plans..\n";
            OK();
        } else if(command == "RECOMMENDATION") {
            sock->send("FOR\n", 4);
            long user = 0;
            memset(buffer, '\0', RCVBUFSIZE);
            if(sock->recv(buffer, RCVBUFSIZE) > 0 && (user = atol(buffer))) {
                sock->send("WAIT\n", 5);
                SRS::users users = getSRS().getRecommendation(user);
                char userIDString[20];
                for(auto const rec : users) {
                    memset(userIDString, '\0', 20);
                    sprintf(userIDString, "%lu", rec);
                    sock->send(userIDString, strlen(userIDString));
                    sock->send("\n",1);
                }
                OK();
            } else {
                ERROR();
            }
        }
        else if(command == "ACCEPTED RECOMMENDATION") {
            sock->send("FROM\n", 5);
            long from = 0, user = 0;
            memset(buffer, '\0', RCVBUFSIZE);
            if(sock->recv(buffer, RCVBUFSIZE) > 0 && (from = atol(buffer))) {
                sock->send("ACCEPTED USER\n", 14);
                memset(buffer, '\0', RCVBUFSIZE);
                if(sock->recv(buffer, RCVBUFSIZE) > 0 && (user = atol(buffer))) {
                    // TODO: gestire il fatto che from ha accettanto la raccomandazione di user
                    // come varia i parametri? quali parametri? nuova metrica?
                    OK();
                } else {
                    ERROR();
                }
            } else {
                ERROR();
            }
        }
        else if(command == "BYE") {
            sock->send("BYE\n",4);
            break;
        }
        else {
            ERROR();
        }
        memset(buffer, '\0', RCVBUFSIZE);
    }
    delete sock;
}
