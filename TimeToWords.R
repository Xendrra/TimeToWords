TimeToWords<-function(hours){
  #deklarace promennych
  pocetp=nchar(hours)                                          #pocet znaku ve vstupu
  hodiny_cisla=c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24')
  hodiny_slova=c('twelve','one','two','three','four','five','six','seven','eight','nine','ten','eleven','twelve','one')
  minuty_cisla=c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39','40','41','42','43','44','45','46','47','48','49','50','51','52','53','54','55','56','57','58','59')
  minuty_slova=c("o'clock",'one','two','three','four','five','six','seven','eight','nine','ten','eleven','twelve','thirteen','fourteen','quarter','sixteen','seventeen','eighteen','nineteen','twenty','twenty-one','twenty-two','twenty-three','twenty-four','twenty-five','twenty-six','twenty-seven','twenty-eight','twenty-nine','half')
  pomocna_cisla=c(0:59)
  pomoc=100
  temp=numeric()
  chyba='Spatny format vstupu'
  min=""
  hod=""
  # osetreni vstupu:
  if (substring(hours,2,2)==':' | substring(hours,3,3)==':'){  #definuje dvojtecku jako jedine mozne rozdeleni minut a hodin
    if (pocetp==5){                                            #rozdeli spravne minuty a hodiny pro pet znaku ve vstupu
      hodiny=substring(hours, 1,2)
      minuty=substring(hours, 4,5) 
    }else if (pocetp==4){                                      #rozdeli spravne minuty a hodiny pro 4 znaky ve vstupu
      hodiny=substring(hours, 1,1)
      minuty=substring(hours, 3,4)
    }else{                                                     #pokud nema vstup 4 nebo 5 znaku, vypise chybu
      print(chyba)
    }
    #zavedeni pomocneho ciselneho vektoru
    for (i in 1:60){                                           #"prevede" minuty ze stringu na cislo
      if (minuty==minuty_cisla[i]){
        pomoc=pomocna_cisla[i]
      }
    }
    # prevod minut a definovani spojky
    for (i in 1:60){
      if (pomoc>0 & pomoc<=30){                               #od 1 do 30 minut zavadi spojku past
        spojka='past'
        if (minuty==minuty_cisla[i]){                          #pro dane cislo v minutach pripojuje dane cislo jako string
          min=minuty_slova[i]
          print(i)
          if (i>2 & i<=15){                                   #pro minuty od :02 do :14 zavadi minutes 
            min=paste(min, "minutes")
          }else if (i>=17 && i<=30){ 
            min=paste(min, "minutes")                        #pro minuty od :16 do :29 zavadi minutes
          }else if(i==2){ 
            min=paste(min,'minute')                         #pro minuty :01 zavadi slovo minute
          }}
      }else if (pomoc==0){                                      # pro minutu v hodnote 0 definuje slovo oclock
        spojka=""
        min="o'clock"
      }else if (pomoc>30 && pomoc<60){                       #od 31 do 59 minut zavadi spojku to
        spojka='to'
        if (minuty==minuty_cisla[i]){                         #od 31 do 59 minut zavadi odecet od 60
          index=60-pomocna_cisla[i]
          min=minuty_slova[index+1]
          if (index>1 && index<=14){                          #pro minuty od :02 do :14 zavadi slovo minutes
            min=paste(min, "minutes")
          }else if (index>=16 && index<30){                  #pro vsechny minuty od :16 do :29 zavadi minutes                     
            min=paste(min, "minutes")                     
          }else if(index==1){ 
            min=paste(min,"minute")                        #pro minutu :59 zavadi minute
          }else{
            min=min                                        #pro zbytek (:00, :15, :30, :45) neudava zadne slovo navic
          }
        }
      }else {                                              #pokud hodnota promenne pomoc nespada do vektoru 0 az 59, promenna min se zustane prazdna 
        min=""
      }}
    #prevod hodin a definovani pripony AM/PM
    for (i in 1:24){                                           
      if (hodiny==hodiny_cisla[i]){                       #vypise poradi dane hodiny ve vektoru hodiny_cisla 
        temp=i                                            
        if (temp>12){                                     #pokud je poradi vetsi nez 12, odecte se od nej 12, aby se vesel do vektoru hodiny_slova
          i=i-12                                          #zaroven se pro tuto hodnotu priradi pripona PM
          pripona='PM.'
        }else{                                            #poradi nizsi nez 12 ziskava priponu AM
          pripona="AM."
        }
        if (pomoc<=30){                                   #podle minut se urci, zda pouzije zadanou hodinu, nebo hodinu nasledujici 
          hod=hodiny_slova[i]
        }else{
          hod=hodiny_slova[i+1]
        }}
    }
    #vypis casu/chyby
    if (min=="" | hod==""){                               #pokud je promenna min nebo hod prazdna, vyhodi chybu     
      print(chyba)
    #urceni slovosledu:
    }else if (min=="o'clock"){                                 
      if (pripona=="AM."){
        print(paste("It's", hod, min))
      }else if(pripona=="PM."){
        print(paste("It's", hod, pripona))
      }}
    else {
      print(paste("It's",min,spojka,hod,pripona))
    }}
  else{                                                #pokud vstup neodpovida pocatecni podmince, haze chybu
    print(chyba)
  }}