#ifndef FILECHECKER_PUBLISH_H
#define FILECHECKER_PUBLISH_H

class myMosq {
  private:
   const char * host;
   const char * id;
   const char * topic;
   int port;
   int keepalive;

   struct mosquitto *mosq; 
  
   void on_connect(int rc);
   void on_disconnect(int rc);
   void on_publish(int mid);
  public:
    myMosq(const char *id, const char * _topic, const char *host, int port);
   ~myMosq();
   bool send_message(const char * _message);
};

#endif //FILECHECKER_PUBLISH_H
