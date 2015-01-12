#!/bin/bash

for i in  Activities ChannelBanners ChannelSections Channels GuideCategories I18nLanguages I18nRegions PlaylistItems Playlists Search Subscriptions Thumbnails VideoCategories Videos Watermarks;
do

#echo -e "module Google.Api.Youtube.$i where\n\n -- https://developers.google.com/youtube/v3/docs/#$i\n" > "$i.hs"
echo "Google.Api.Youtube.$i"

done