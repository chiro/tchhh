{-# LANGUAGE OverloadedStrings #-}

module Secret (
  tokens
  ) where

import Web.Authenticate.OAuth

tokens :: OAuth
tokens = def { oauthServerName = "twitter"
             , oauthRequestUri = "http://twitter.com/oauth/request_token"
             , oauthAccessTokenUri = "http://twitter.com/oauth/access_token"
             , oauthAuthorizeUri = "http://twitter.com/oauth/authorize"
             , oauthConsumerKey = "JBCyIp0oW2IppbybQ5VIw"
             , oauthConsumerSecret = "YMfHA02sd4dbFn8jmxvtKNZSl8ZCGzZYS649RuT4YE"
             , oauthSignatureMethod = HMACSHA1
             , oauthCallback = Nothing
             }
