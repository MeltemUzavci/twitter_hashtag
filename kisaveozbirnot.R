# #kisaveozbirnot hashtagi ile at�lan tweetlerin basit�e analizi

#Tweetlerin �ekilmesi
kisaveozbirnot <- search_tweets(q = "#K�saVe�zBirNot",
                                n = 5000)

#Lokasyona g�re t�m tweet atanlar�n say�s�n�n grafiklendirilmesi
ggplot(kisaveozbirnot) +geom_bar(aes(x=location)) + 
  coord_flip() +
  labs(x = "Tweet atanlar�n say�s�",
       y = "Location",
       title = "Twitter kullan�c�lar� - e�siz locations ")


#Lokasyona g�re ilk 20 tweet atanlar�n say�s�n�n grafiklendirilmesi
kisaveozbirnot %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(20) %>%
  ggplot() +
  geom_col(aes(x = location, y = n)) +
  coord_flip() +
  labs(x = "Tweet atanlar�n say�s�",
       y = "Location",
       title = "Twitter kullan�c�lar� - e�siz locations")


#verinin isimlerinin t�rk�ele�tirilmesi
kisaveozbirnot_temel= kisaveozbirnot %>%
  select(tweetler=text, tweet_atan_kisi=screen_name,tweet_atilan_yer=location)



#t�m tweetlerin k���k harfe �evirilmesi
kisaveozbirnot_temel$tweetler=sapply(kisaveozbirnot_temel$tweetler, tolower)

library(tm)
kisaveozbirnot_temel$tweetler=sapply(kisaveozbirnot_temel$tweetler,stripWhitespace)#tm paketi gerekli ve birden fazla bo�luk varsa kald�r�r. 


kisaveozbirnot_temel$tweetler=sapply(kisaveozbirnot_temel$tweetler,removePunctuation)   #noktalama i�aretlerini metinden kald�r�r.



kisaveozbirnot_temel$tweetler=gsub("K�saVe�zBirNot","",kisaveozbirnot_temel$tweetler)      #gsub istedi�imiz kelimelerin yerini isted�imiz �eyle de�i�tirmemizi sa�lar.


#http ile ba�layan linkleri temizler
kisaveozbirnot_temel$tweetler= gsub("http.*","",kisaveozbirnot_temel$tweetler)

# b�y�k � harfini k����e �evirir. 
kisaveozbirnot_temel$tweetler= gsub("�","i",kisaveozbirnot_temel$tweetler)



#veriden stopwordslerin ��kar�lmas� ve 3 harften k���k kelimelern silinmesi
kisaveozbirnot_temel_kelime <- kisaveozbirnot_temel %>%
  unnest_tokens(word, tweetler) %>%
  filter(!word %in% atilacak_kelimeler) %>%
  filter(nchar(word)>3)

#Verideki her kelimenin ge�me say�s�n�n hesaplanmas�
kisaveozbirnot_temel_kelime_sayisi=kisaveozbirnot_temel_kelime %>%
  count(word,sort=TRUE)


#verinin kelime bulutunun olu�turulmas�
wordcloud2(kisaveozbirnot_temel_kelime_sayisi[1:300, ],size=2,
           shape="star",backgroundColor = "black",color="random-light")
