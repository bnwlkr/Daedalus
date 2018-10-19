module View where


import Codec.Picture



view =
        do
            imageCreator "hello.png"
            print "3"


imageCreator path = writePng path $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
