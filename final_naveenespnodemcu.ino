#include <DHT.h>  
#include <SoftwareSerial.h>
 #include <ESP8266WiFi.h>  
 #include <WiFiClient.h>  
 #include <ThingSpeak.h>  
 #define DHTPIN D5  
 #define encoder_pin  D2
 #define DHTTYPE DHT11  
 DHT dht(DHTPIN, DHTTYPE);  
 const char* ssid = "iPhone";  
 const char* password = "123456789";  
 const int channelID = 297846;
 String writeAPIKey = "LOUDHGMXFTSPYPPR";
 const char* server = "api.thingspeak.com";
 WiFiClient client;  
 unsigned long myChannelNumber = 297846;  
 const char * myWriteAPIKey = "LOUDHGMXFTSPYPPR";  
 uint8_t temperature, humidity;  
 float rpm;
  String body="";
  
int b=0,c,i=0;

 void setup()  
 {  
  Serial.begin(115200);  
  dht.begin();  
  delay(10);  
  // Connect to WiFi network  
  Serial.println();  
  Serial.println();  
  Serial.print("Connecting to ");  
  Serial.println(ssid);  
  WiFi.begin(ssid, password);  
  while (WiFi.status() != WL_CONNECTED)  
  {  
   delay(500);  
   Serial.print(".");  
  }  
  Serial.println("");  
  Serial.println("WiFi connected");  
  // Print the IP address  
  Serial.println(WiFi.localIP());  
  ThingSpeak.begin(client);  
 }  
 void loop()   
 {  
  static boolean data_state = false;  
  temperature = dht.readTemperature();  
  humidity = dht.readHumidity();  
  Serial.print("Temperature Value is :");  
  Serial.print(temperature);  
  Serial.println("C");  
  Serial.print("Humidity Value is :");  
  Serial.print(humidity);  
  Serial.println("%");  
  // Write to ThingSpeak. There are up to 8 fields in a channel, allowing you to store up to 8 different  
  // pieces of information in a channel. Here, we write to field 1.  
  if( data_state )  
  {  
   ThingSpeak.writeField(myChannelNumber, 1, temperature, myWriteAPIKey);  
   data_state = false;  
  }  
  else  
  {  
   ThingSpeak.writeField(myChannelNumber, 2, humidity, myWriteAPIKey);  
   data_state = true;  
  }  
  delay(30000); // ThingSpeak will only accept updates every 15 seconds.  




if (client.connect(server, 80))
{
// Measure Analog Input (A0)
int x = analogRead(A0);
// Construct API request body
 body = "&field3=";
body += String(x);

//Serial.print("A0: ");
//Serial.println(valueA0);
client.print("POST /update HTTP/1.1\n");
client.print("Host: api.thingspeak.com\n");
client.print("Connection: close\n");
client.print("X-THINGSPEAKAPIKEY: " + writeAPIKey + "\n");
client.print("Content-Type: application/x-www-form-urlencoded\n");
client.print("Content-Length: ");
client.print(body.length());
client.print("\n\n");
client.print(body);
client.print("\n\n");
Serial.print(body);
delay(30000);
}


if (client.connect(server, 80))
{rpm=0;
// Measure Analog Input (A0)
for(i=3;i>0;i--)
  { 
    delay(1000);


     
    int a=digitalRead(encoder_pin);
    if(a==1)
    c=c+1;
    Serial.println(i);
  }
 
   if(i==0)
   {  rpm=c;
      Serial.print("RPM = ");
      Serial.println(rpm);
      delay(200);
      i=3;
      
   }  
   
// Construct API request body
 body = "&field4=";
body += String(rpm);

//Serial.print("A0: ");
//Serial.println(valueA0);
client.print("POST /update HTTP/1.1\n");
client.print("Host: api.thingspeak.com\n");
client.print("Connection: close\n");
client.print("X-THINGSPEAKAPIKEY: " + writeAPIKey + "\n");
client.print("Content-Type: application/x-www-form-urlencoded\n");
client.print("Content-Length: ");
client.print(body.length());
client.print("\n\n");
client.print(body);
client.print("\n\n");
Serial.print(body);
delay(30000);
rpm=0;
}
  
 }  
