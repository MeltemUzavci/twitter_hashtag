# #kisaveozbirnot hashtagi ile atýlan tweetlerin basitçe analizi

#Tweetlerin çekilmesi
kisaveozbirnot <- search_tweets(q = "#KýsaVeÖzBirNot",
                                n = 5000)

#Lokasyona göre tüm tweet atanlarýn sayýsýnýn grafiklendirilmesi
ggplot(kisaveozbirnot) +geom_bar(aes(x=location)) + 
  coord_flip() +
  labs(x = "Tweet atanlarýn sayýsý",
       y = "Location",
       title = "Twitter kullanýcýlarý - eþsiz locations ")


#Lokasyona göre ilk 20 tweet atanlarýn sayýsýnýn grafiklendirilmesi
kisaveozbirnot %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(20) %>%
  ggplot() +
  geom_col(aes(x = location, y = n)) +
  coord_flip() +
  labs(x = "Tweet atanlarýn sayýsý",
       y = "Location",
       title = "Twitter kullanýcýlarý - eþsiz locations")


#verinin isimlerinin türkçeleþtirilmesi
kisaveozbirnot_temel= kisaveozbirnot %>%
  select(tweetler=text, tweet_atan_kisi=screen_name,tweet_atilan_yer=location)



#tüm tweetlerin küçük harfe çevirilmesi
kisaveozbirnot_temel$tweetler=sapply(kisaveozbirnot_temel$tweetler, tolower)

library(tm)
kisaveozbirnot_temel$tweetler=sapply(kisaveozbirnot_temel$tweetler,stripWhitespace)#tm paketi gerekli ve birden fazla boþluk varsa kaldýrýr. 


kisaveozbirnot_temel$tweetler=sapply(kisaveozbirnot_temel$tweetler,removePunctuation)   #noktalama iþaretlerini metinden kaldýrýr.



kisaveozbirnot_temel$tweetler=gsub("KýsaVeÖzBirNot","",kisaveozbirnot_temel$tweetler)      #gsub istediðimiz kelimelerin yerini istedðimiz þeyle deðiþtirmemizi saðlar.


#http ile baþlayan linkleri temizler
kisaveozbirnot_temel$tweetler= gsub("http.*","",kisaveozbirnot_temel$tweetler)

# büyük Ý harfini küçüðe çevirir. 
kisaveozbirnot_temel$tweetler= gsub("Ý","i",kisaveozbirnot_temel$tweetler)



#veriden stopwordslerin çýkarýlmasý ve 3 harften küçük kelimelern silinmesi
kisaveozbirnot_temel_kelime <- kisaveozbirnot_temel %>%
  unnest_tokens(word, tweetler) %>%
  filter(!word %in% atilacak_kelimeler) %>%
  filter(nchar(word)>3)

#Verideki her kelimenin geçme sayýsýnýn hesaplanmasý
kisaveozbirnot_temel_kelime_sayisi=kisaveozbirnot_temel_kelime %>%
  count(word,sort=TRUE)


#verinin kelime bulutunun oluþturulmasý
wordcloud2(kisaveozbirnot_temel_kelime_sayisi[1:300, ],size=2,
           shape="star",backgroundColor = "black",color="random-light")
