library(httr)
library(rjson)
library(RCurl)
library(yaml)
library(RColorBrewer)

# For this to authenticate properly, you must have your default browser open.
# A page will come up and ask you to authorize if the token has expired.

#use_proxy("proxy.useastgw.xerox.com", port = 8000)

# Note: the redirect URI in Mendeley *has* to be http://localhost:1410/
mendeley <- oauth_endpoint(base_url = 'https://api.mendeley.com/oauth', 
                           authorize = 'authorize', access = 'token')
myapp <- oauth_app(appname = 'funWithSOTL', key = "2363", secret = "j3zBZPNaHRMMtox1")
token <- oauth2.0_token(mendeley, myapp, scope='all',cache=TRUE)

profile_rsp <- GET('https://api.mendeley.com/profiles/me', config(token = token))

profile <- fromJSON(rawToChar(content(profile_rsp)))
message(paste('Hello, ', profile$display_name, '!', sep=''))

#### NOW I am logged in.  go get data.
groups <- GET('https://api.mendeley.com/groups', config(token = token))
groups_content <- fromJSON(rawToChar(content(groups)))

documents <- GET(paste0('https://api.mendeley.com/documents?group_id=',
                        groups_content[[1]]$id), config(token = token))
documents_content <- fromJSON(rawToChar(content(documents)))



doi <- readline('Enter a DOI: ')
doc_rsp <- GET(paste('https://api.mendeley.com/catalog?view=stats&doi=', curlEscape(doi), sep=''), config(token = token))
docs <- fromJSON(rawToChar(content(doc_rsp)))

if (length(docs) > 0) {
  doc <- docs[[1]]
  message(paste(doc$title, 'has', doc$reader_count, 'readers.'))
  
  df <- data.frame(t(data.frame(doc$reader_count_by_academic_status)))
  colnames(df) <- c('readers')
  par(mar=c(5,18,1,1), las=2)
  barplot(df$readers, names.arg=row.names(df), horiz=TRUE, col=brewer.pal(n=15, name="Reds"))
} else {
  message('Document not found.')
}
