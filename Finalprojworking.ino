#define trigPin1 8
#define echoPin1 A2    
#define trigPin2 11
#define echoPin2 A3
#define trigPin3 12
#define echoPin3 A1
#define mfront 3
#define mback 6
#define mleft 9
#define mright 10
#define seat_belt 5 

String voice;


int smokeA0 = A0;   //alcohol

const int left_right_set=15;
const int setspeed=153;
int a=setspeed;
char ch,state,c;
const int setdistance=150;
const int safedistance=90;
const int criticaldistance=70;
const int headlight=2;
const int backlight=4;
long duration, distance, RightSensor,BackSensor,FrontSensor,LeftSensor;


      void setup(){
              Serial.begin (9600);
              pinMode(headlight, OUTPUT);
              pinMode(backlight, OUTPUT);
              pinMode(trigPin1, OUTPUT);
              pinMode(echoPin1, INPUT);
              pinMode(trigPin2, OUTPUT);
              pinMode(echoPin2, INPUT);
              pinMode(trigPin3, OUTPUT);
              pinMode(echoPin3, INPUT);
              pinMode(mfront, OUTPUT);
              pinMode(mback, OUTPUT);
              pinMode(mleft, OUTPUT);
              pinMode(mright, OUTPUT);
              pinMode(smokeA0, INPUT);
              
                  }

      void loop(){  
            digitalWrite(backlight,LOW);
           
            int seat=digitalRead(seat_belt);
              while(Serial.available()>0)
              {                //voice
                   delay(10);
                    char c=Serial.read();
                    if(c=="engine on")
                      {break; }
                     voice += c;
              }
             if(voice=="engine on")
             {
               
               digitalWrite(headlight,HIGH);
               checkdistance(trigPin3, echoPin3);
               FrontSensor = distance;
               Serial.print(FrontSensor);
               Serial.print("cm\n");
               
            if(Serial.available()>0){
                 ch=(char) Serial.read();
                 Serial.print(ch);
             }
             // Manual Mode
          while(ch == 'a' ){                                                  //While1 Open
                   if(Serial.available()>0){
                     state = (char) Serial.read();  
                     if(state == 'b'){
                     ch = state;
                      }
                   }
             else if(state == 'u'){
                  Serial.print("Up Up Up Up Up Up Up Up"); //Up Control
                  Serial.print(state);
                  analogWrite(mfront, setspeed);   
                  analogWrite(mback,setspeed);a=setspeed;
                  digitalWrite(backlight,LOW);
                }
             else if(state == 's'){
                  Serial.print("Stop Stop Stop Stop Stop"); //Stop
                  Serial.print(state);
                  digitalWrite(mfront, 0); 
                  digitalWrite(mback,0); 
                  digitalWrite(mleft, 0);   
                  digitalWrite(mright,0); a=mfront;
                  digitalWrite(backlight,HIGH);
                  }
            else if(state == 'l'){
                 Serial.print("Left Left Left Left Left");    //Left Control
                 Serial.print(state);
                 digitalWrite(mfront, 0);  
                 analogWrite(mback,setspeed); a=mback;
                  digitalWrite(backlight,LOW);
                  }
            else if(state ==  'r'){
                 Serial.print("Right Right Right Right Right");   //Right Control
                 Serial.print(state);                                     
                 analogWrite(mfront, setspeed);  
                 digitalWrite(mback,0);    a=mback;
                  digitalWrite(backlight,LOW);
                  }
            else if(state == 'd') {
                Serial.print("Down Down Down Down Down");       //Down Control
                Serial.print(state);                             
                analogWrite(mleft, setspeed);   
                analogWrite(mright,setspeed);a=mleft;   
                  digitalWrite(backlight,LOW);
                  }
    }                                                                    //While1 Close
    
          //Acc Mode
            if(Serial.available()>0){
                 ch=(char) Serial.read();
                 Serial.print(ch);
             }
                     if(ch == 'b'){                                           // Acc Loop Open
                        
                        checkdistance(trigPin1, echoPin1);
                        RightSensor = distance;
                        checkdistance(trigPin2, echoPin2);
                        LeftSensor = distance;
                        checkdistance(trigPin3, echoPin3);
                        FrontSensor = distance;
                        Serial.print(FrontSensor);
                        Serial.print("cm\n");
                            if(FrontSensor>110){ 
                             analogWrite(mfront, setspeed);   
                             analogWrite(mback,setspeed);
                           // analogWrite(mleft, setspeed);   
                           // analogWrite(mright,setspeed);
                           digitalWrite(backlight,LOW); 
                            a=setspeed; 
                        }
                 else if(FrontSensor>setdistance)                 //actual distance is more  accelerate
                {
                    while(FrontSensor!=setdistance){
                         for(int i=a;i<=153;i++){
                              a=i;
                              analogWrite(mfront, a);   
                              analogWrite(mback, a);  
                            //  analogWrite(mleft, a);
                            digitalWrite(backlight,LOW);   
                            }
                            break;
                      } 
                  }
                 else{                                             //actual distance is more  deaccelerate
                    while(FrontSensor!=setdistance){
                      for(int i=a;i>=127;--i){
                           a=i;
                          analogWrite(mfront, a);   
                          analogWrite(mback, a); 
                         // analogWrite(mleft, a);   
                           // analogWrite(mright,a);  
                           
                        }
                        break;
                    }
                     if(FrontSensor<=criticaldistance){              //close proximity alert
                        analogWrite(mfront,0);
                         analogWrite(mright,0);
                         analogWrite(mleft, 0);   
                         analogWrite(mback,0);
                         digitalWrite(backlight,HIGH);
                      }
                 }
                 if(LeftSensor<left_right_set)                      //left and right sensor 
                      {
                         digitalWrite(backlight,HIGH);
                        Serial.print("cut-in left");
                         Serial.print("DRIVER INTERVENTION NEEDED");
                      }
                      if(RightSensor<left_right_set)
                      {
                         digitalWrite(backlight,HIGH);
                        Serial.print("cut-in right");
                         Serial.print("DRIVER INTERVENTION NEEDED");
                      }
           }                                                          //Acc loop close

      }                                                                //voice close
     /* if(c!='h')      {
        Serial.print("voice recognition failed");
      }*/                                                          
      }                                                                //loop close
void checkdistance(int trigPin,int echoPin)
{
digitalWrite(trigPin, LOW);
delayMicroseconds(2);
digitalWrite(trigPin, HIGH);
delayMicroseconds(10);
digitalWrite(trigPin, LOW);
duration = pulseIn(echoPin, HIGH);
distance = (duration/2) / 29.1;

}
