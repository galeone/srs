#include "SRS.h"
#include "../include/PracticalSocket.h"
#include <algorithm> // std::find_if_not
#include <sstream>
#include <cstdio>
#include <pthread.h>

using namespace std;
using namespace srs;

#define ALPHA 4
#define BASE_MAX 10000.0

const int RCVBUFSIZE = 32;
void *handleRequest(void *sock);     // TCP client handling function

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
    unsigned short port = 0;
    if (argc != 2) {
        cout << "[+] Using default port: 9876" << endl;
        port = 9876;
    } else {
        port = atoi(argv[1]);
        if(!port) {
            cerr << "Usage: " << argv[0] << " <Server Port> " << endl;
            return EXIT_FAILURE;
        }
    }
    TCPServerSocket servSock(port);  // Socket descriptor for server

    try {
        setlocale(LC_ALL, "en_US.utf8"); // Set locale (for wcstring)
        PlEngine engine(argc, argv); // Init Prolog engine
        for (;;) {
            pthread_t tid;
            TCPSocket *clntSock = servSock.accept();
            if(pthread_create(&tid, NULL, handleRequest, (void *)clntSock) != 0) {
                cerr << "Unable to create thread " << endl;
                return EXIT_FAILURE;
            }
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

inline void printStat(int actual_user, int true_positive, int true_negative, int false_positive, int false_negative) {
    cout << "[+] \tTesting for user " << actual_user << "\n";
    cout << "[+] Building confusion matrix...\n";
    cout << "\tActual class:\tFollowed\tNot followed\n";
    cout << "Predicted class\tFollowed\t" << true_positive << "\t" << false_positive << "\n\t\tNot followed\t" << false_negative << "\t" << true_negative <<"\n";

    if(true_positive + false_negative > 0) {
        cout << "Sensivity: " << (float)true_positive/(true_positive + false_negative) << "\n";
        cout << "False negative rate: " << (float)false_negative/(false_negative + true_positive) << "\n";
    }
    if(false_positive + true_negative > 0) {
        float spc = (float)true_negative/(false_positive + true_negative);
        cout << "Specificity: " << spc << "\n";
        cout << "Fall out: " << (1 - spc) << "\n";
    }
    if(true_positive + false_positive > 0) {
        float ppv = (float)true_positive/(true_positive + false_positive);
        cout << "Precision: " << ppv << "\n";
        cout << "False discovery rate: " << (1 - ppv) << "\n";
    }
    if(true_negative + false_negative > 0) {
        cout << "Negative predictive value: " << (float)true_negative/(true_negative + false_negative)  << "\n";
    }

    if(true_positive + false_positive + false_negative > 0) {
        cout << "F1 score: " << (2*(float)true_positive/(2*true_positive + false_positive + false_negative)) << "\n";
    }
}

// TCP client handling function
void * handleRequest(void *params) {
    TCPSocket *sock = (TCPSocket *)params;
    char buffer[RCVBUFSIZE];
    memset(buffer, '\0', RCVBUFSIZE);
    //http://www.swi-prolog.org/pldoc/man?section=foreignthread
    if(PL_thread_attach_engine(NULL) < 0) {
        cerr << "PL_thread_attach_engine error" << endl;
        return NULL;
    }

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
            SRS::users all_users = getSRS().getUsers();
            SRS::users test_users = {1898, 581, 574, 376, 817,740, 244, 1814,448,403, 352, 1788};
            auto au_s = test_users.size();
            vector<int> true_positive(au_s,0); // followed and recommended to follow
            vector<int> false_positive(au_s, 0); // not followed but recommendted to follow
            vector<int> false_negative(au_s, 0); // followed but not recommended to follow
            vector<int> true_negative(au_s, 0);// not followed and not recommended to be followed

            for(int i=0; i<au_s;++i) {
                auto actual_user = test_users[i];

                SRS::users following = getSRS().getFollowing(actual_user);
                SRS::users_rank affinity = getSRS().getUsersSortedByAffinity(actual_user);

                for(auto const& other_user : all_users) {
                    auto it = find_if(affinity.begin(), affinity.end(), [&](pair<long, float> p) {
                            return p.first == other_user;
                            });
                    bool recommended_to_follow = it != affinity.end();

                    if(find(following.begin(), following.end(), other_user) != following.end()) {
                        // followed
                        if(!recommended_to_follow) {
                            // and not recommended
                            ++false_negative[i];
                        } else {
                            // and recommended
                            ++true_positive[i];
                        }
                    } else {
                        // not followed
                        if(!recommended_to_follow) {
                            // and not recommented
                            ++true_negative[i];
                        } else {
                            // and recommented
                            ++false_positive[i];
                        }
                    }
                }
                printStat(actual_user, true_positive[i], true_negative[i], false_positive[i], false_negative[i]);
            }

            cout << "\n\n";
            int tp = 0, tn=0, fp=0, fn=0;
            for(int i=0;i<au_s;i++) {
                tp += true_positive[i];
                tn += true_negative[i];
                fp += false_positive[i];
                fn += false_negative[i];
            }

            printStat(-1, tp, tn, fp, fn);

            OK();
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
    PL_thread_destroy_engine();
    return NULL;
}
