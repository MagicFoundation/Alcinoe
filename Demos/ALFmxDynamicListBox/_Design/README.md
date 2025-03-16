# Download Sample Posts for Instagram  

You can use [Instaloader](https://instaloader.github.io) to download sample posts from Instagram.  

## Steps  

1. First, log in to Instagram using **Firefox** to initialize the cookies.  
2. Then, run the following command:  

   ```
   instaloader --fast-update --no-compress-json --load-cookies Firefox --user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:135.0) Gecko/20100101 Firefox/135.0" :saved
   ```

This will download posts from the **saved collection** in your Instagram account.


# Extract the first frame of a video

ffmpeg -i input.mp4 -vf "select=eq(n\,0)" -q:v 2 -frames:v 1 first_frame.jpg