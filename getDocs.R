# get document list from Mendeley in pages, then convert to data frame
# and write to a csv file.
# This process needs only to be done when the Mendeley group is updated.
# Otherwise it is more efficient to work from the output csv file (documentMetaData.csv)
library(httr)
library(rjson)
source("preprocessUtils.R")

# Log in and validate.  
# For this to authenticate properly, you must have your default browser open.
# A page will come up and ask you to authorize...  Right now it is using Fritz's 
# secret key... so the auth web page will ask you to put in my credentials...
mendeley <- oauth_endpoint(base_url = 'https://api.mendeley.com/oauth', 
                           authorize = 'authorize', access = 'token')
myapp <- oauth_app(appname = 'funWithSOTL', key = "2363", secret = "j3zBZPNaHRMMtox1")
token <- oauth2.0_token(mendeley, myapp, scope='all',cache=TRUE)
profile_rsp <- GET('https://api.mendeley.com/profiles/me', config(token = token))
profile <- fromJSON(rawToChar(content(profile_rsp)))
message(paste('Hello, ', profile$display_name, '!', sep=''))
yn <- readline('Did you see the proper Hello message? (y,n)')
if (yn != 'y') stop ()
#######################
# 1 get group ID from group name 'Mgt Ed - SOTL project'
# NOTE: Need to replace spaces with '%20' to get it to work.
groupUrl <- 'https://api.mendeley.com/groups?name=Mgt%20Ed%20-%20SOTL%20project'
groups <- GET(groupUrl, config(token = token))
groups_content <- fromJSON(rawToChar(content(groups)))
SOTL_groupID <- groups_content[[1]]$id
#######################

# 2 Loop over these to read 500 at a time.
# parse link to see if 1) there is one, and 2) if it is rel=next
# keep going til there are no more 'next' links.
# We initially create a list of return objects (which are lists)
docListList <- list()

counter <- 1
print(paste("retriving page",counter))
# do the initial GET.  This returns link to the next page
docUrl <- paste0('https://api.mendeley.com/documents?limit=500&group_id=', SOTL_groupID)
documents <- GET(docUrl, config(token = token))
docListList[[counter]] <- fromJSON(rawToChar(content(documents)))

while(is.nextLink(documents$headers$link)) {
    counter <- counter + 1
    print(paste("retriving page",counter))
    theUrl <- getUrlFromLink(documents$headers$link)
    documents <- GET(theUrl, config(token = token))
    docListList[[counter]] <- fromJSON(rawToChar(content(documents)))
}

docList <- unlist(docListList, recursive = FALSE)
docList.df <- doclistToDataframe(docList)
write.csv(docList.df,file = "documentMetaData.csv")