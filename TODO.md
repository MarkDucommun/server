# TODOs
* [ ] Write tests that send client requests with Content-Length and no new-lines  
* [ ] Write tests that send client requests with new-lines and no Content-Length  
* [ ] Audit all `<-` in IO blocks
    * [ ] `main.hs:41`
    * [ ] `Client.hs:19`
    * [ ] `Client.hs:21`
    * [ ] `Client.hs:35`
    * [ ] `Client.hs:37`
    * [ ] `ClientResponse.hs:21`
    * [ ] `ClientResponse.hs:41`
    * [ ] `ClientResponse.hs:53`
    * [ ] `ClientResponse.hs:69`
    * [ ] `ClientResponse.hs:76`
    * [ ] `ClientResponse.hs:80`
    * [ ] `ClientResponse.hs:88`
    * [ ] `ClientResponse.hs:89`
    * [ ] `RequestBuilder.hs:13`
    * [ ] `RequestBuilder.hs:17`
    * [ ] `RequestBuilder.hs:22`
    * [ ] `RequestBuilder.hs:27`
    * [ ] `RequestBuilder.hs:32`
    * [ ] `RequestBuilder.hs:37`
    * [ ] `RequestBuilder.hs:42`
    * [ ] `RequestBuilder.hs:51`
    * [ ] `RequestBuilder.hs:52`
    * [ ] `RequestBuilder.hs:53`
    * [ ] `RequestBuilder.hs:84`
    * [ ] `RequestBuilder.hs:85`
    * [ ] `RequestBuilder.hs:92`
    * [ ] `RequestBuilder.hs:110`
    * [ ] `RequestBuilder.hs:116`
    * [ ] `Server.hs:31` - probably print message and then shut down gracefully
    * [ ] `Server.hs:36`
    * [ ] `Server.hs:37`
    * [ ] `Server.hs:45`
    * [ ] `Server.hs:50`
    * [ ] `ResponseWriter.hs:11`
    * [ ] `ResponseWriter.hs:12`
* [ ] Extract `getContentLength`    
* [ ] Extract `getBodyByLength`    
* [ ] Extract `getBodyByEmptyLine` 

## Completed   
* [x] Clean up request builder
    * [x] `getRequest`
    * [x] `createRequestBuilder`