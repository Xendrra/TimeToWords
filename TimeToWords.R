TimeToWords<-function(hours){
  pocetp=nchar(hours)
  minuty=""
  hodiny_cisla=c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24')
  hodiny_slova=c('twelve','one','two','three','four','five','six','seven','eight','nine','ten','eleven','twelve','one')
  minuty_cisla=c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39','40','41','42','43','44','45','46','47','48','49','50','51','52','53','54','55','56','57','58','59')
  minuty_slova=c("o'clock",'one','two','three','four','five','six','seven','eight','nine','ten','eleven','twelve','thirteen','fourteen','quarter','sixteen','seventeen','eighteen','nineteen','twenty','twenty-one','twenty-two','twenty-three','twenty-four','twenty-five','twenty-six','twenty-seven','twenty-eight','twenty-nine','half')
  pomocna_cisla=c(0:60)
  pomoc=numeric()
  spojka=""
  temp=numeric()
  hod=""
  min=""
  chyba='Spatny format vstupu'
  if (substring(hours,2,2)==':' | substring(hours,3,3)==':'){  #definuje dvojtecku jako jedine mozne rozdeleni minut a hodin
    if (pocetp==5){                                            #rozdeli spravne minuty a hodiny pro pet znaku ve vstupu
      hodiny=substring(hours, 1,2)
      minuty=substring(hours, 4,5) 
    }else if (pocetp==4){                                      #rozdeli spravne minuty a hodiny pro 4 znaky ve vstupu
      hodiny=substring(hours, 1,1)
      minuty=substring(hours, 3,4)
    }else{                                                     #pokud nema vstup 4 nebo 5 znaku, vypise chybu
      print(chyba)
      print('prvni else')
    }
    for (i in 1:60){                                           #"prevede" minuty ze stringu na cislo
      if (minuty==minuty_cisla[i]){
        pomoc=pomocna_cisla[i]
      }else { pomoc=100
    }
    }
    for (i in 1:60){
      if (pomoc>0 && pomoc<=30){                               #od 1 do 30 minut zavadi spojku past
        spojka='past'
        if (minuty==minuty_cisla[i]){                          #pro dane cislo v minutach pripojuje dane cislo jako string
          min=minuty_slova[i]
        }}
      else if (pomoc==0){                                      # pro minutu v hodnote 0 definuje slovo oclock
        spojka=""
        min="o'clock"
      }else if (pomoc>30 && pomoc<60){                       #od 31 do 59 minut zavadi spojku to
        spojka='to'
        print(pomoc)
        if (minuty==minuty_cisla[i]){                         #od 31 do 59 minut zavadi odecet od 60
          index=60-pomocna_cisla[i]
          min=minuty_slova[index+1]
        }else {print(chyba)
          print(ananas)}}                                                #pro jine hodnoty minut nez 0:59 vypise c
    }
    for (i in 1:24){                                           
      if (hodiny==hodiny_cisla[i]){
        temp=i
        if (temp>12){
          i=i-12}
        if (pomoc<=30){
          hod=hodiny_slova[i]}
        else{
          hod=hodiny_slova[i+1]
        }
      }
    }
    # if (min=="" | hod==""){ 
    #   print(chyba)
    #   print('kokos')} 
    # else{ 
      print(min)
      print(spojka)
      print(hod)
    # }
  }
  else{
    print(chyba)
  }
}
