import os,json,urllib.request as U
W=0,100,320,330,500,900,0
N=18,14,33,31,-18,-14,-33,-31
O=15,17,-15,-17
Z=2,3,4,5
L8=1,-1,16,-16,15,17,-15,-17
S8=[x for x in range(128)if not x&136]
T={}
CM=[15]*128;CM[0]=13;CM[4]=12;CM[7]=14;CM[112]=7;CM[116]=3;CM[119]=11
LIM=28000
def LD(t):
 global b,s,e,c,K
 a=t.split();b=[0]*128;f=112
 for h in a[0]:
  if h=='/':f-=24
  elif h<':':f+=int(h)
  else:
   v='.PNBRQKpnbrqk'.index(h);b[f]=v if v<7 else 6-v;f+=1
 s=1-2*(a[1]=='b')
 h=a[2];c=('K'in h)+2*('Q'in h)+4*('k'in h)+8*('q'in h)
 e=-1 if a[3]=='-' else ord(a[3][0])-97+16*(int(a[3][1])-1)
 K=[b.index(6),b.index(-6)]
def A(q,s):
 for d,p in((N,2),(L8,6),(O[2:]if s>0 else O[:2],1)):
  for x in d:
   t=q+x
   if not t&136 and b[t]==p*s:return 1
 for d,p in(L8[:4],(4,5)),(O,(3,5)):
  for x in d:
   t=q+x
   while not t&136 and not b[t]:t+=x
   if not t&136 and b[t]*s>0 and abs(b[t])in p:return 1
 return 0
def M(s,q=0):
 l=[]
 for f in S8:
  p=b[f]
  if p*s<1:continue
  a=abs(p)
  if a==1:
   if not q:
    t=f+16*s
    if not t&136 and not b[t]:
     l+=[f|t<<7|x<<14 for x in Z if t>111 or t<8]or[f|t<<7]
     if f>>4==(1,6)[s<0]and not b[t+16*s]:l+=[f|(f+32*s)<<7]
   for x in O[:2]if s>0 else O[2:]:
    t=f+x
    if not t&136 and -6<b[t]*s<0:l+=[f|t<<7|y<<14 for y in Z if t>111 or t<8]or[f|t<<7]
    elif t==e>-1:l+=f|t<<7,
  elif a==2:
   for x in N:
    t=f+x
    if not t&136 and b[t]*s<=0 and(not q or b[t]):l+=f|t<<7,
  elif a==6:
   for x in L8:
    t=f+x
    if not t&136 and(b[t]*s<0 or not q and b[t]*s<=0):l+=f|t<<7,
   if not q:
    for w,o,h,k in((1,4,6,7),(2,4,2,0),(4,116,118,119),(8,116,114,112)):
     if c&w and not any(b[min(o,k)+1:max(o,k)])and not any(A(x,-s)for x in range(min(o,h),max(o,h)+1)):l+=o|h<<7,
  else:
   for x in(0,0,N,O,L8[:4],L8,O)[a]:
    t=f+x
    while not t&136:
     y=b[t]
     if y:
      if y*s<0:l+=f|t<<7,
      break
     if not q:l+=f|t<<7,
     t+=x
 return l
def V(x):
 global s,e,c
 f=x&127;t=x>>7&127;p=x>>14;q=b[f];y=b[t];d=t-f
 w=b[:],s,e,c,K[:]
 b[f]=0;b[t]=p*q or q
 if q*q<4:
  if t==e>-1 and not y:b[t-16*q]=0
  e=(f+t)>>1 if d*d==1024 else -1
 else:e=-1
 if q*q==36:
  K[q<0]=t
  if abs(d)==2:
   h=t+1 if d>0 else t-2;y=t-1 if d>0 else t+1
   b[y]=b[h];b[h]=0
 c&=CM[f]&CM[t];s=-s
 return w
def RS(w):
 global s,e,c
 b[:],s,e,c,K[:]=w
def LEG(s):
 l=[]
 for x in M(s):
  w=V(x)
  if not A(K[s<0],-s):l+=[x]
  RS(w)
 return l
def E(s):
 v=w=0
 for f in S8:
  p=b[f]
  if p:
   a=abs(p);x=3-abs(f%8-3)
   if a==1:x+=f//16-1 if p>0 else 6-f//16
   v+=p*(W[a]+4*x)if a<6 else-4*p*x
   w+=p*(a==3)
 return v*s+(w>1)*30-(w<-1)*30
def P(d,al,be,s,p):
 global n,RT
 n+=1
 if n>=LIM:1/0
 if d<1:
  t=E(s)
  if t>=be:return t
  if t>al:al=t
  if p>16:return al
  m=0;l=M(s,1)
 else:
  k=tuple(b),s,e,c
  z=T.get(k);m=0;ao=al
  if z:
   if z[0]>=d and(not z[2]or z[2]<2 and z[1]<=al or z[1]>=be):return z[1]
   m=z[3]
  l=M(s)
 o=[(x==m,W[abs(b[x>>7&127])],x)for x in l]
 o.sort(reverse=True)
 bs=-24000;bm=0
 for _,v,x in o:
  w=V(x)
  if A(K[s<0],-s):RS(w);continue
  v=-P(d-1,-be,-al,-s,p+1);RS(w)
  if v>bs:
   bs=v;bm=x
   if not p:RT=x
  if v>al:al=v
  if al>=be:break
 if d<1:return al
 if bs<-20000:return(-24000+p)*A(K[s<0],-s)
 if bs*bs<4e8:
  T[k]=d,bs,(bs<=ao)+2*(bs>=be),bm
 return bs
def C(x):
 f=x&127;t=x>>7&127
 return f'{chr(97+f%8)}{f//16+1}{chr(97+t%8)}{t//16+1}'+('..nbrq'[x>>14]if x>>14 else'')
def B(fs,lm):
 global n,RT
 LD(fs);n=0;RT=0;T.clear()
 for d in range(60):
  o=RT
  try:v=P(d,-99999,99999,s,0)
  except:RT=o;break
  if v*v>4e8:break
 r=C(RT)
 return r if r in lm else lm[0]
def I(p,d):
 return json.load(U.urlopen(U.Request(X+p,json.dumps(d).encode(),{'Content-Type':'application/json'}),timeout=8))
def Y():
 while 1:
  try:q=I('/game',{})
  except:return
  if q['status']=='done':return
  k=q['game']
  while q['status']=='your_move':
   try:m=B(q['fen'],q['legal_moves'])
   except:m=q['legal_moves'][0]
   try:q=I('/move',{'game':k,'move':m})
   except:break
if __name__=='__main__':
 X=os.environ['MAKEFUN_ENV_URL'];Y()
