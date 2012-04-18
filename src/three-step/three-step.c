int main(int argv, char * argc[]){
	int x = atoi(argc[1]);
	int p1 = 0;
	int p2 = 0;
	int p3 = 0;
	 //p1 = 7;
	 //p2 = 3;
	 //p3 = 4;
	int now = p1+p2+p3;
	if( x == 1 ){
		printf("%d:%d:%d\n",x,p1-p2-p3,now==p1+p2+p3);
	}
	if( x == 2 ){
		printf("%d:%d:%d\n",x,p1-p2-p3,now==p1+p2+p3);
	}
	if( x == 3 ){
		printf("%d:%d:%d\n",x,p1-p2-p3,now==p1+p2+p3);
	}
	if( x == 4 ){
		printf("%d:%d:%d\n",x,p1-p2-p3,now==p1+p2+p3);
	}
	if( x == 5 ){
		printf("%d:%d:%d\n",x,p1-p2-p3,now==p1+p2+p3);
	}
	if( x == 666) {
		printf("%d:%d:%d:%d\n",x,p1+p2+p3,p1-p2-p3,now==p1+p2+p3);
	}
	p1 = 7;
	p2 = 3;
	p3 = 4;
}
