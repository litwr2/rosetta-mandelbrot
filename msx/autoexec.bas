 10 clear 16,&h84f8
 20 screen 0:width 80:print "1. 128x256, 16c, interlaced (mode 256x424)"
 30 print "2. 256x128, 16c, rotated (mode 256x192)"
 40 print "3. 512x212, 16c (fullscreen)"
 50 print "4. 512x424, 16c (fullscreen, interlaced)"
 60 print "5. 256x212, 256c (fullscreen)"
 70 print "6. exit"
 80 input a$:a=val(a$):if a<1 or a>6 then 20
 90 on a gosub 110,120,130,140,150,100:bload a$,r:goto 20
100 end
110 a$="m128x256":return
120 a$="m256x128":return
130 a$="m512x212":return
140 a$="m512x424":return
150 a$="m256x212":return
