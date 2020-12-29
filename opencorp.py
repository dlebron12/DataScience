from urllib2 import Request, urlopen, HTTPError
import json 
import pandas as pd
import matplotlib
matplotlib.use("Agg")
from matplotlib import pyplot as plt

def opencorp_query(search_term, page=1):
    qurl = "https://api.opencorporates.com/v0.4/companies/search?q=" + \
      search_term + "&page=" + str(page) #Form the query url
    try:
        request = Request(qurl)
        response = urlopen(request) #grab the API output
        return response.read()
    except HTTPError:
        return False


search_term = "google"
comp_json = json.loads(opencorp_query(search_term))
numpages = comp_json['results']['total_pages'] #get the number of pages
google = pd.DataFrame()
for i in range(numpages-1):
    try:
        comp_json = json.loads(opencorp_query(search_term,page=i+1)) 
        #convert the returned string to dict
    except:
        continue
    if comp_json:
        names = [c['company']['name'] for c in comp_json['results']['companies']]
        created_at = [c['company']['created_at'] for c in comp_json['results']['companies']]
        jur = [c['company']['jurisdiction_code'] for c in comp_json['results']['companies']]
        inactive = [c['company']['inactive'] for c in comp_json['results']['companies']]
        google = google.append(pd.DataFrame({'name':names, 'created_at':created_at,
                               'jurisdiction':jur, 'inactive':inactive}))
    
num_rec = google.shape[0]
us_ind = google["jurisdiction"].str.startswith("us")
labels = "US active", "US inactive", "Foreign active", "Foreign inactive", "Missing"
bool_coef = [(True,False),(True,True),(False,False),(False, True)]
sizes = [((us_ind==a[0]) & (google["inactive"]==a[1])).sum() for a in bool_coef]
sizes.append(num_rec - sum(sizes))
plt.pie(sizes,labels=labels)
plt.savefig("googlepie.png")
