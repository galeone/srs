#include "SRS.h"
#include "../include/PracticalSocket.h"
#include <algorithm> // std::find_if_not
#include <sstream>
#include <cstdio>

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
#define WAIT() sock->send("WAIT\n",5)

// TCP client handling function
void handleRequest(TCPSocket *sock) {
    char buffer[RCVBUFSIZE];
    memset(buffer, '\0', RCVBUFSIZE);

    while(sock->recv(buffer, RCVBUFSIZE) > 0) {
        cout << "[!] Received command: " << buffer << "\n";
        string command(buffer);
        command = trim(command);
        if(command == "UPDATE") {
            WAIT();
            getSRS().updateDB();
            cout << "[+] Database updated..\n";
            getSRS().generatePlans();
            cout << "[+] Generated plans..\n";
            OK();
        } else if(command == "STORE PLANS") {
            // can't use wofstream because fstream is crap
            auto dataset = fopen("dataset.csv", "w");       
            wostringstream ws;
            ws << L"plan, x, y, user\n";
            for(const auto& plansit : getSRS().getPlans()) {
                for(const auto& it : plansit.second) {
                    ws << plansit.first + L", ";
                    ws << get<0>(it.second);
                    ws << L", ";
                    ws << get<1>(it.second);
                    ws << L", ";
                    ws << it.first;
                    ws << L"\n";
                }
            }
            fputws(ws.str().c_str(), dataset);
            fclose(dataset);
            OK();  
        } else if(command == "RUN TESTS") {
            WAIT();
            cout << "[+] Updating db...\n";
            getSRS().updateDB();
            cout << "[+] Generating plans...\n";
            getSRS().generatePlans();
            long user = 1;
            SRS::users following = getSRS().getFollowing(user); // ordered by follow time
            SRS::users_rank recommendation = getSRS().getRecommendation(user);

            auto recButNo = 0;
            for(int i=0;i<recommendation.size(); i++) {
                if(find(following.begin(), following.end(), get<0>(recommendation[i]))  == following.end()) {
                    recButNo++;
                }
            }
            cout << "Compiting differences between recommendation and following\n";
            cout << "Reccomendation size: " << recommendation.size() << "\n";
            cout << "Following size: " << following.size() << "\n";
            cout << "Recommended but not followed: " << recButNo << "\n";
            cout << "Recommended and followed: " << recommendation.size() - recButNo << "\n";
            // Find true positive (correct match) (found), false positive and find precision
            // Find true positive and false negative to find recall (accuracy)

        } else if(command == "RECOMMENDATION") {
            sock->send("FOR\n", 4);
            long user = 0;
            memset(buffer, '\0', RCVBUFSIZE);
            if(sock->recv(buffer, RCVBUFSIZE) > 0 && (user = atol(buffer))) {
                WAIT();
                SRS::users_rank users_r = getSRS().getRecommendation(user);
                char userIDString[20];
                for(auto const &rec : users_r) {
                    memset(userIDString, '\0', 20);
                    sprintf(userIDString, "%lu", get<0>(rec));
                    sock->send(userIDString, strlen(userIDString));
                    sock->send(" ",1);
                    memset(userIDString, '\0', 20);
                    sprintf(userIDString, "%f", get<1>(rec));
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
