import pandas as pd
import numpy as np
import scipy
from scipy.sparse import csr_matrix
from scipy.sparse import linalg as sslinalg
#read “enwiki-2013.txt” into a sparse CSR matrix
#nodes=pd.read_table("enwiki-2013.txt",sep=" ", skiprows=4, names=("from","to"))
#names=pd.read_csv("enwiki-2013-names.csv", skiprows=1, sep=",", header=None)
nodes=pd.read_table("enwiki-2013.txt",sep=" ", skiprows=4, names=("from","to"))
names=pd.read_csv("enwiki-2013-names.csv", skiprows=1, sep=",", header=None)

#Implement the power method to compute the top singular value
#and corresponding right singular vector for a given CSR matrix
global n
n=max(nodes["from"])+1
vals = np.ones(len(nodes["from"]))
csr=csr_matrix((vals,(nodes["from"],nodes["to"])), shape=(n,n))


def power_method(A, i):
    np.random.seed(seed=50)
    k=1
    B=np.transpose(A).dot(A)
    #Calculate a random normal vector
    x=np.random.rand(n)
    #Calculate power
    while k <= i:
        x=B.dot(x)
        x=x/np.linalg.norm(x)
        k=k+1
    #Final x will then be the unit vector after k iterations of the power method
        s=B.dot(x).dot(x)/(x.dot(x))
    return x,s  #top right singular vector and top singular value

v,s=power_method(csr,10)
#2 Quality of solution *********
def Calc_quality(v, A):
    v=np.asmatrix(v)
    B=np.transpose(A).dot(A)
    qual=(v*B).dot(np.transpose(v))
    return qual

#what is the range for other v?
np.random.seed(seed=25)
v=np.random.rand(n)
Calc_quality(v,csr)

#3
iterators=[1,3,5,10,20]
svd_v=[]
svd_e=[]
svd_qual=[]
v_ls=[]
e_ls=[]
power_qual=[]


for k in iterators:
    %timeit power_method(csr,k)
    v,e= power_method(csr,k)
    v_ls=v_ls.append(v)
    e_ls=e_ls.append(e)
    power_qual.append(Calc_quality(v,csr))

#Calculate singular vector with SVD
    %timeit scipy.sparse.linalg.svds(csr,k, return_singular_vectors="vh")
    U,S,V=scipy.sparse.linalg.svds(csr,k, return_singular_vectors="vh")
    svd_v.append(V)
    svd_e.append(S)
    svd_qual.append(Calc_quality(V,csr))


#V is the unitary matrix having right singular vectors as rows
#which one is better? ******

#4: Authority score of web page; list names of top 5 authoritive pages and scores
#The top 5 authoritive pages will come from the k=5 singular values.
v,e= power_method(csr,5)
top= v.argsort()[-5:][::-1]
for k in top:
    print names[1][k]

#5: Calculate hub-score

#6:Compute page rank
from sklearn.preprocessing import normalize
P = normalize( csr, norm='l1', axis=1)
e=np.ones(n)
page_rank=0.9*np.transpose(P)+(0.1/n)*e.dot(np.transpose(e))
