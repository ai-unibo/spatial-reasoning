#include <cstdlib>
#include <cstring>
#include <iostream>
#include <opencv2/opencv.hpp>
#include <opencv2/text.hpp>
#include <regex>
#include <tesseract/baseapi.h>
#include "/usr/lib/swi-prolog/include/SWI-Prolog.h"
#include "/usr/lib/swi-prolog/include/SWI-cpp.h"


using namespace cv;
using namespace std;

#define _DEBUG
#define _WIN32
//#define TESSDATA "/usr/share/tesseract-ocr/4.00/tessdata"

#if defined(_DEBUG) && defined(_WIN32)	//per non far bloccare le finestre
void window(const char* n);
//void alert(const char* n);
static bool first=true;
#endif // _DEBUG

#ifdef _WIN32
#include <signal.h>
void SignalHandler(int signal){		//to catch double release_img error
	cout<<"Signal "<<signal<<endl;
	throw "Access Violation!";
}
#endif



//******************** Utility ********************//
#define IMG(img) PlCompound("img", PlTermv(PlTerm(img)))
#define P(x,y) PlCompound("p", PlTermv(PlTerm((long)(x)), PlTerm((long)(y))))
#define RGB(r,g,b) PlCompound("rgb", PlTermv(PlTerm((long)(r)), PlTerm((long)(g)), PlTerm((long)(b))))
#define Vec2RGB(v) RGB(v[2],v[1],v[0])
void polyToVertexVector(PlTerm const &poly, vector<Point> &v){
//WAS: void polyToVertexVector(PlTerm &poly, vector<Point> &v){  (but this gives "Initial value of reference to non-const must be an lvalue")
	PlTail lv(poly[2]);
	PlTerm p;
	v.resize(0);
	while (lv.next(p))
		v.push_back(Point((int)(double)p[1], (int)(double)p[2]));
}
PlTerm vertexVectorToPoly(vector<Point> &v){
	PlTerm poly;
	PlTail lv(poly);
	for (Point &p : v)
		lv.append(P(p.x, p.y));
	lv.close();
    // return a term like:
    // poly(3,[p(217,75),p(269,129),p(273,109)])
	return PlCompound("poly", PlTermv(PlTerm((long)v.size()), poly));
}
int debug_enabled(){
	return PlCall("(current_prolog_flag(debug,State),call(State))");
}
int web_enabled(){
	return PlCall("(current_prolog_flag(web,State),call(State))");
}

PREDICATE0(check_installed){
	return TRUE;
}

//******************** IO ********************//
PREDICATE(load_img, 2){	//load_img(+Path,-Img)
    //PlCall("writeln",PlTerm(getenv("TESSDATA_PREFIX")));
	string file(A1);
	Mat* img=new Mat(imread(file));
	if (img->data==NULL){
		cerr<<"Impossibile aprire il file "<<file<<endl;
		PlCall("writeln('Can\\'t open file')");
		return FALSE;
	}
	return A2=IMG(img);
}
PREDICATE(write_img, 2){	//write_img(+Path,+Img)
	string file(A1);
	if (strcmp(A2.name(), "img")|| A2.arity()!=1)
		return PlCall("type_error", PlTermv(PlTerm("img"), A2));
	Mat* img=(Mat*)(void*)A2[1];
	imwrite(file, *img);
	return TRUE;
}
PREDICATE(release_img, 1){	//release_img(+Img)
#ifdef _WIN32
	signal(SIGSEGV, SignalHandler);
#endif
	if (strcmp(A1.name(), "img") || A1.arity()!=1)
		return PlCall("type_error", PlTermv(PlTerm("img"), A1));
	try {
		delete (Mat*)(void*)A1[1];
	} catch (exception e){}
	return TRUE;
}
//******************** Image manipulation ********************//
PREDICATE(subimg, 4){	//subimg(+Img,+Poly,-SubImg,-POrig)
	if (strcmp(A1.name(), "img") || A1.arity()!=1)
		return FALSE;
	Mat* img=(Mat*)(void*)A1[1];	//PlTerm args index starts from 1
	if (strcmp(A2.name(), "poly") || A2.arity()!=2)
		return FALSE;
	vector<Point> vv;
	polyToVertexVector(A2, vv);
	int xMin=10000, yMin=10000, xMax=0, yMax=0;
	for (Point &p : vv){
		xMin=MIN(xMin, p.x);
		yMin=MIN(yMin, p.y);
		xMax=MAX(xMax, p.x);
		yMax=MAX(yMax, p.y);
	}
	Mat* roi=new Mat((*img)(Rect(Point(xMin, yMin), Point(xMax, yMax))));
	return (A3=IMG(roi)) && (A4=P(xMin,yMin));
}

PREDICATE(copy_img, 2){	//copy_img(?Img1,?Img2)
	Mat *img1=NULL, *img2=NULL;
	if (A1.type()!=PL_VARIABLE && strcmp(A1.name(), "img")==0 && A1.arity()==1)
		img1=(Mat*)(void*)A1[1];
	if (A2.type()!=PL_VARIABLE && strcmp(A2.name(), "img")==0 && A2.arity()==1)
		img2=(Mat*)(void*)A2[1];
	if (img1)
		if (img2){
			if (img1!=img2 && img1->type()==img2->type()&&img1->size()==img2->size())
				return memcmp(img1->data, img2->data, img1->rows*img1->step)==0;
			return FALSE;
		} else {
			img2=new Mat;
			img1->copyTo(*img2);
			return A2=IMG(img2);
		}
	else
		if (img2){
			img1=new Mat;
			img2->copyTo(*img1);
			return A1=IMG(img1);
		} else
			return FALSE;
}

void drawObjects(Mat &img, PlTail l, Scalar color=Scalar(), int lineSize=1){
	PlTerm obj;
	while (l.next(obj))
		if (strcmp(obj.name(), "p")==0&&obj.arity()==2)
			if (lineSize==1)
				for (int k=0;k<img.channels();k++)
					img.data[(img.rows*(int)(double)obj[2]+(int)(double)obj[1])*img.channels()+k]=(uchar)color[k];
				//img.at<uchar>(Point(obj[1], obj[2]))=0;
			else
				circle(img, Point((int)(double)obj[1], (int)(double)obj[2]), (lineSize+1)>>1, color, FILLED);
		else if (strcmp(obj.name(), "seg")==0 && obj.arity()==2)
			line(img, Point((int)(double)obj[1][1], (int)(double)obj[1][2]), Point((int)(double)obj[2][1], (int)(double)obj[2][2]), color, lineSize);
		else if (strcmp(obj.name(), "poly")==0 && obj.arity()==2){
			vector<Point> vv;
			polyToVertexVector(obj, vv);
			for (int i=0;i<vv.size();i++)
				line(img, vv[i], vv[(i+1)%vv.size()], color, lineSize);
		}
}
PREDICATE(draw_objects_, 4){	//drawObjects(?Img,+List,+Color,+LineSize)
	Mat* img;
	if (strcmp(A1.name(), "img")==0 && A1.arity()==1)
		img=(Mat*)(void*)A1[1];
	else if (A1.type()==PL_VARIABLE){
		img=new Mat(~Mat::zeros(1000, 500, CV_8U));
		if (!(A1=IMG(img))){
			delete img;
			return FALSE;
		}
	} else
		return FALSE;
	if (!PlCall("is_list", PlTermv(A2)))
		return FALSE;
	Scalar color;
	if (strcmp(A3.name(), "rgb")||A3.arity()!=3)
		return FALSE;
	if (A4.type()!=PL_INTEGER && A4.type()!=PL_FLOAT)
		return FALSE;
	drawObjects(*img, PlTail(A2), Scalar((int)A3[3], (int)A3[2], (int)A3[1]), (int)A4);
	return TRUE;
}


#ifdef _DEBUG
void initWindows(){
	if (first){
		window("Image");
		if (debug_enabled())
			window("Contour");
		first=false;
	}
}

PREDICATE(show_img, 1){	//show_img(+Img)
    if (! web_enabled() ){ // the image is shown only on console mode
        if (strcmp(A1.name(), "img")||A1.arity()!=1)
            return FALSE;
        Mat* img=(Mat*)(void*)A1[1];
        initWindows();
        imshow("Image", *img);
        waitKey(0);
    }
	return TRUE;
}
PREDICATE(show_objects, 1){	//show_objects(+List)
	if (!PlCall("is_list", PlTermv(A1)))
		return FALSE;
	Mat img=~Mat::zeros(1000, 500, CV_8U);
	drawObjects(img, PlTail(A1));
	initWindows();
	imshow("Image", img);
	waitKey(1);
	return TRUE;
}

//******************** Draw example images ********************//
void draw_rect(Mat& img, int x, int y, int w, int h, Scalar colore){
	line(img, Point(x, y), Point(x+w, y), colore);
	line(img, Point(x, y), Point(x, y+h), colore);
	line(img, Point(x+w, y), Point(x+w, y+h), colore);
	line(img, Point(x, y+h), Point(x+w, y+h), colore);
}
PREDICATE(draw_rect, 1){
	int scale=100;
	Mat img=Mat::ones(3*scale+1, 4*scale+1, CV_8UC1)*255;
	Scalar black(0, 0, 0);
	draw_rect(img, 0, 0, 4*scale, 3*scale, black);
	draw_rect(img, scale, scale, 2*scale, scale, black);
	line(img, Point(2*scale, scale), Point(2*scale, 2*scale), black);
	return A1=IMG(new Mat(img));
}
PREDICATE(draw_grid, 1){
	int r=3, c=2, scale=100, margin=10;
	Mat img=Mat::ones(r*scale+1+margin*2, c*scale+1+margin*2, CV_8UC1)*255;
	Scalar black(0, 0, 0);
	for (int y=0;y<=r*scale;y+=scale)
		line(img, Point(margin, margin+y), Point(margin+c*scale, margin+y), black, 2);
	for (int x=0;x<=c*scale;x+=scale)
		line(img, Point(margin+x, margin), Point(margin+x, margin+r*scale), black, 2);
	return A1=IMG(new Mat(img));
}
PREDICATE(draw_grid_triangle, 1){
	int r=3, c=2, scale=100, margin=10;
	Mat img=Mat::ones(r*scale+1+margin*2, c*scale+1+margin*2, CV_8UC1)*255;
	Scalar black(0, 0, 0);
	for (int y=0;y<=r*scale;y+=scale)
		line(img, Point(margin, margin+y), Point(margin+c*scale, margin+y), black, 2);
	for (int x=0;x<=c*scale;x+=scale)
		line(img, Point(margin+x, margin), Point(margin+x, margin+r*scale), black, 2);
	for (int x=0;x<=(c+r)/2*scale;x+=scale)
		line(img, Point(margin+x, margin), Point(margin, margin+x), black, 2);
	return A1=IMG(new Mat(img));
}
PREDICATE(draw_circles, 1){
	int scale=50, margin=5;
	Mat img=Mat::ones(4*scale+1+margin*2, 7*scale+1+margin*2, CV_8UC1)*255;
	Scalar black(0, 0, 0);
	circle(img, Point(2*scale+margin, 2*scale+margin), scale, black, 3);
	circle(img, Point(2*scale+margin, 2*scale+margin), scale*2, black, 3);
	circle(img, Point(6*scale+margin, 2*scale+margin), scale, black, 3);
	return A1=IMG(new Mat(img));
}
#endif // _DEBUG

//******************** color ********************//
PREDICATE(get_color_rgb, 3){	//get_color_rgb(+Img,+Point,-Color)
	if (strcmp(A1.name(), "img")||A1.arity()!=1)
		return FALSE;
	if (strcmp(A2.name(), "p")||A1.arity()!=2)
		return FALSE;
	Mat img=*(Mat*)(void*)A1[1];	//PlTerm args index starts from 1
	Point p(A2[1], A2[2]);
	Vec3b color;
	if (img.channels()==3)
		color=img.at<Vec3b>(p);
	else
		memset(&color, img.at<uchar>(p), 3);
	return A2=Vec2RGB(color);
}
PREDICATE(main_color_rgb, 2){	//main_color_rgb(+Img,-Color)
	if (strcmp(A1.name(), "img")||A1.arity()!=1)
		return FALSE;
	Mat img=*(Mat*)(void*)A1[1];	//PlTerm args index starts from 1
	int ch=img.channels();
	int dim[]={256,256,256};
	Mat hist=Mat::zeros(ch, dim, CV_32S);
	int nmax=0, ncur;
	Vec3b max, cur;
	for (int i=0;i<img.rows;i++)
		for (int j=0;j<img.cols;j++){
			if (ch==1)
				memset(&cur, img.at<uchar>(i, j), 3);
			else
				cur=img.at<Vec3b>(i, j);
			hist.at<int>(cur[0], cur[1], cur[2])++;
			ncur=hist.at<int>(cur[0], cur[1], cur[2]);
			if (ncur>nmax){
				nmax=ncur;
				max=cur;
			}
		}
	return A2=Vec2RGB(max);
}


//******************** shapes ********************//
PREDICATE(find_all_polygons, 4){	//find_all_polygons(+Img,+BorderSize,+Threshold,-PolyList)
	if (strcmp(A1.name(), "img")||A1.arity()!=1)
		return FALSE;
	Mat img, *input=(Mat*)(void*)A1[1];	//PlTerm args index starts from 1
	if (input->channels()>1)
		cvtColor(*input, img, COLOR_BGR2GRAY);
	else
		input->copyTo(img);
	int th=A3;
	if (th==0){
		th=255;
		for (int i=0;i<img.rows;i++)
			for (int j=0;j<img.cols;j++)
				th=MIN(th, img.at<uchar>(i, j));
		th+=100;
		th=MAX(th, 150);
	} else 	if (th==255){
		th=0;
		for (int i=0;i<img.rows;i++)
			for (int j=0;j<img.cols;j++)
				th=MAX(th, img.at<uchar>(i, j));
		th-=30;
	}
	//if (debug_enabled())
	//	PlCall("writeln", PlTermv(PlTerm((long)th)));
	threshold(img, img, MAX(th,1), 255, th<0?THRESH_OTSU:THRESH_BINARY);
    vector<vector<Point>> contours, lVertices;
	vector<Vec4i> hierarchy;
	int kSize=((int)A2)*2-1;
	if (kSize>1){
		Mat imgInv;
		erode(img, imgInv, Mat::ones(kSize, kSize, CV_8U));
		imgInv.copyTo(img);
	}
	//dilate(imgInv, img, Mat::ones(3, 3, CV_8U));
	findContours(img, contours, hierarchy, RETR_LIST, CHAIN_APPROX_SIMPLE);
	for (vector<Point> &contour:contours){
		vector<Point> vertices;
		//double perimeter=arcLength(contour, true);
        //Approximates a polygonal curve(s) with precision 2
		approxPolyDP(contour, vertices, /*perimeter*.04*/2, true);
		//vertices=contour;
		if (vertices.size()==4){
			bool border=true;
			for (Point &p:vertices)
				if ((p.x>0 && p.x<img.cols-2)||(p.y>0 && p.y<img.rows-2)){
					border=false;
					break;
				}
			if (border)
				continue;
		}
		if (vertices.size()>2)// && /*vertices[0].cross(vertices[1])>0)//*/(vertices[1]-vertices[0]).cross(vertices[2]-vertices[1])<0)
			lVertices.push_back(vertices);
	}
#ifdef _DEBUG
	if (debug_enabled()){
		initWindows();
		//cout<<contours.size()<<endl;
		//cout<<lVertices.size()<<endl;
		Mat img2=~Mat::zeros(img.size(), CV_8UC3);
        static Scalar colore[]={Scalar(0,0,255),Scalar(0,127,127),Scalar(0,255,0),Scalar(127,127,0),Scalar(255,0,0),Scalar(0,0,0)};
		for (int i=0; i<lVertices.size(); i++)
			drawContours(img2, lVertices, i, colore[lVertices[i].size()-3]);
		imshow("Image", img);
		imshow("Contour", img2);
		waitKey(1);
	}
#endif // _DEBUG
	PlTerm r;
	PlTail l(r);
	for (vector<Point> &v:lVertices)
        // composing a list of poly prolog terms. It is something like:
        // [poly(3,[p(275,113),p(272,132),p(300,161)]),poly(3,[p(217,75),p(269,129),p(273,109)]), ...
		l.append(vertexVectorToPoly(v));
	l.close();
	return A4=r;
}

PREDICATE(area, 2){	//area(+Poly,-Area)
	if (strcmp(A1.name(), "poly")||A1.arity()!=2)
		return FALSE;
	vector<Point> v;
	polyToVertexVector(A1, v);
	return A2=PlTerm(contourArea(v));
}
/*PREDICATE(find_circles, 2){

}*/
//******************** puzzle ********************//

//******************** text ********************//
using namespace cv::text;

class TextMatch {
public:
	string text;
	Rect bounds;
	float probability, fontSize;
	TextMatch(){}
	TextMatch(string text, Rect bounds, float probability, float fontSize=0){
		this->text=text;
		this->bounds=bounds;
		this->probability=probability;
		this->fontSize=fontSize;
	}
};

#undef _DEBUG
void findText(Mat &img, vector<TextMatch> &res, int xMax=10000){
	//static Ptr<BaseOCR> ocr=OCRTesseract::create(TESSDATA, "ita+ita_old+fra+eng"/*+osd"*/, "0123456789", OEM_CUBE_ONLY, PSM_SINGLE_COLUMN);
    static Ptr<BaseOCR> ocr=OCRTesseract::create(NULL, "ita+ita_old+fra+eng"/*+osd"*/, "0123456789", OEM_CUBE_ONLY, PSM_SINGLE_COLUMN);
	string text;
	vector<string> texts;
	vector<Rect> textRects;
	vector<float> prob;
	ocr->run(img, text, &textRects, &texts, &prob, 0);
#ifdef _DEBUG
	if (debug_enabled()){
		initWindows();
		//cout<<text<<endl;
		Mat img2;
		cvtColor(img, img2, COLOR_GRAY2BGR);
	}
#endif // _DEBUG
	res.resize(0);
	regex empty("[\\s\\v]*");
	regex lines("[\\s\\v]*[\\|_]+[\\s\\v]*");
    regex alphanum("^[a-zA-Z0-9()]*$");
	for (int i=0; i<texts.size(); i++){
		bool ok=!(textRects[i].x>xMax||textRects[i].width<5||textRects[i].height<5||prob[i]<50||regex_match(texts[i], empty)||regex_match(texts[i], lines) || !regex_match(texts[i], alphanum));
		if (ok){
			//cout<<texts[i]<<": "<<prob[i]<<"%\n";
            string s = string(texts[i])+string(": ")+to_string(prob[i])+string("%");
            PlCall("writeln",PlTerm(s.c_str()));
			res.push_back(TextMatch(texts[i], textRects[i], prob[i]));
		}
#ifdef _DEBUG
		if (debug_enabled()){
			//putText(img2, texts[i], textRects[i].tl(), FONT_HERSHEY_COMPLEX_SMALL, 1, Scalar(color, color, color));
			rectangle(img2, textRects[i], ok?Scalar(255, 0, 0):Scalar(0, 0, 255));
		}
#endif // _DEBUG
	}
#ifdef _DEBUG
	if (debug_enabled()){
		imshow("Image", img);
		imshow("Contour", img2);
		waitKey(1);
	}
#endif // _DEBUG
}
void findTextMultiscale(Mat &img, vector<TextMatch> &matches, float minScale, float maxScale){
	vector<TextMatch> curMatches;
	matches.resize(0);
	for (double fontSize=minScale;fontSize<=maxScale;fontSize+=.1){
		int rh=(int)(30*fontSize);
		Mat imgExt=~Mat::zeros(img.rows, img.cols+(int)(500*fontSize), img.type());
		img.copyTo(imgExt(Rect(Point(0, 0), img.size())));
		for (int i=0;i<img.rows/rh;i+=5){
			putText(imgExt, "testo di prova", Point(img.cols+10, (i+1)*rh), FONT_HERSHEY_TRIPLEX, fontSize, Scalar(0));
			putText(imgExt, "Qualcosa da scrivere", Point(img.cols+10, (i+2)*rh), FONT_HERSHEY_COMPLEX, fontSize, Scalar(0));
			putText(imgExt, "anche se non so cosa", Point(img.cols+10, (i+3)*rh), FONT_HERSHEY_COMPLEX, fontSize, Scalar(0));
			putText(imgExt, "ciao ciao.", Point(img.cols+10, (i+4)*rh), FONT_HERSHEY_COMPLEX, fontSize, Scalar(0));
			putText(imgExt, "1 2 3 4 5 6 7 8 9", Point(img.cols+10, (i+5)*rh), FONT_HERSHEY_DUPLEX, fontSize, Scalar(0));
		}
#ifdef _DEBUG
		if (debug_enabled())
			cout<<"\n\ndimensione font: "<<fontSize<<endl;
#endif // _DEBUG
		findText(imgExt, curMatches, img.cols);
		regex anyNumber(".*\\d.*");
		for (TextMatch &m:curMatches){
			m.fontSize=(float)fontSize;
			bool already=false;
			for (int i=0;!already && i<matches.size();i++){
				double a1=matches[i].bounds.area(), a2=m.bounds.area(), ai=(m.bounds&matches[i].bounds).area();
				if (ai>a1/2||ai>a2/2){
					already=true;
					if (m.probability>matches[i].probability && regex_match(m.text, anyNumber))
						matches[i]=m;
				}
			}
			if (!already)
				matches.push_back(m);
		}
		//waitKey(1000);//////////////////////////////////////////
	}
#ifdef _DEBUG
	if (debug_enabled()){
		Mat img2;
		cvtColor(img, img2, COLOR_GRAY2BGR);
		for (TextMatch &m:matches){
			cout<<m.text<<": "<<m.probability<<"%\n";
			rectangle(img2, m.bounds, Scalar(255, 0, 0));
			putText(img2, m.text+"("+to_string((int)(m.fontSize*10))+")", m.bounds.tl(), FONT_HERSHEY_COMPLEX_SMALL, .6, Scalar(0));
		}
		imshow("Image", img);
		imshow("Contour", img2);
		waitKey(1);
	}
#endif // _DEBUG
}
PREDICATE(read_single_number, 4){	//read_text(+Img,-Text,-Rect,-FontSize)
	if (strcmp(A1.name(), "img")||A1.arity()!=1)
		return FALSE;
	Mat img, *input=(Mat*)(void*)A1[1];	//PlTerm args index starts from 1
	if (input->channels()>1)
		cvtColor(*input, img, COLOR_BGR2GRAY);
	else
		img=*input;
	//vector<TextMatch> matches;
	TextMatch match("", Rect(), 0);
	const int margin=0;
	const float sMin=MAX((img.rows+margin*2)/45.0f, 0.5f), sMax=MIN(MAX(sMin, (img.rows+margin*2)/30.0f), 2);
	//PlCall("writeln", PlCompound("range", PlTermv(PlTerm(sMin), PlTerm(sMax))));
	/*findTextMultiscale(img, matches, sMin, sMax);
	for (TextMatch &m:matches)
		if (m.probability>match.probability)
			match=m;*/
	vector<TextMatch> curMatches;
	for (double fontSize=sMin;fontSize<=sMax;fontSize+=.1){
		int rh=(int)(30*fontSize);
		Mat imgExt=~Mat::zeros(img.rows+margin*2, img.cols+(int)(500*fontSize), img.type());
		img.copyTo(imgExt(Rect(Point(margin, margin), img.size())));
		putText(imgExt, "testo di prova", Point(img.cols+10, (int)((img.rows+rh)*0.45f)), FONT_HERSHEY_TRIPLEX, fontSize, Scalar(0));        
#ifdef _DEBUG
		//cout<<"\n\ndimensione font: "<<fontSize<<endl;
#endif // _DEBUG
		findText(imgExt, curMatches, img.cols);
		regex anyNumber(".*\\d.*");//("\\d+");
		for (TextMatch &m:curMatches)
			if (m.probability>match.probability /*&& PlCall("writeln", PlTerm(m.text.c_str()))*/ && regex_match(m.text, anyNumber)/* && PlCall("writeln('OK')")*/){
                //DL: removing other chars which are not digits
                m.text=regex_replace(m.text, std::regex(R"([\D])"), "");
				if (debug_enabled())
					PlCall("writeln", PlTerm(m.text.c_str()));
				match=m;
			}
		//waitKey(1000);//////////////////////////////////////////
	}
#ifdef _DEBUG
	if (debug_enabled()){
		Mat img2;
		cvtColor(img, img2, CV_GRAY2BGR);
		cout<<match.text<<": "<<match.probability<<"%\n";
		rectangle(img2, match.bounds, Scalar(255, 0, 0));
		putText(img2, match.text+"("+to_string((int)(match.fontSize*10))+")", match.bounds.tl(), FONT_HERSHEY_COMPLEX_SMALL, .6, Scalar(0));
		imshow("Image", img);
		imshow("Contour", img2);
		waitKey(1);
	}
#endif // _DEBUG*/
	PlTerm poly;
	PlTail lv(poly);
	Rect r=match.bounds;
	lv.append(P(r.x, r.y));
	lv.append(P(r.x, r.y+r.height));
	lv.append(P(r.x+r.width, r.y+r.height));
	lv.append(P(r.x+r.width, r.y));
	lv.close();
	return (A2=PlString(match.text.c_str()))&&(A3=PlCompound("poly", PlTermv(PlTerm((long)4), poly)))&&(A4=PlTerm((long)(match.fontSize*10)));
}
PREDICATE(read_single_text, 4){	//read_text(+Img,-Text,-Rect,-FontSize)
	if (strcmp(A1.name(), "img")||A1.arity()!=1)
		return FALSE;
	Mat img, *input=(Mat*)(void*)A1[1];	//PlTerm args index starts from 1
	if (input->channels()>1)
		cvtColor(*input, img, COLOR_BGR2GRAY);
	else
		img=*input;
	//vector<TextMatch> matches;
	TextMatch match("", Rect(), 0);
	const int margin=0;
	const float sMin=MAX((img.rows+margin*2)/45.0f, 0.5f), sMax=MIN(MAX(sMin, (img.rows+margin*2)/30.0f), 2);
	PlCall("writeln", PlCompound("range", PlTermv(PlTerm(sMin), PlTerm(sMax))));
	/*findTextMultiscale(img, matches, sMin, sMax);
	for (TextMatch &m:matches)
		if (m.probability>match.probability)
			match=m;*/
	vector<TextMatch> curMatches;
	for (double fontSize=sMin;fontSize<=sMax;fontSize+=.1){
		int rh=(int)(30*fontSize);
		Mat imgExt=~Mat::zeros(img.rows+margin*2, img.cols+(int)(500*fontSize), img.type());
		img.copyTo(imgExt(Rect(Point(margin, margin), img.size())));
		putText(imgExt, "testo di prova", Point(img.cols+10, (int)((img.rows+rh)*0.45f)), FONT_HERSHEY_TRIPLEX, fontSize, Scalar(0));
#ifdef _DEBUG
		//cout<<"\n\ndimensione font: "<<fontSize<<endl;
#endif // _DEBUG
		findText(imgExt, curMatches, img.cols);
		regex anyNumber("\\w+");
		for (TextMatch &m:curMatches)
			if (m.probability>match.probability /*&& PlCall("writeln", PlTerm(m.text.c_str()))*/ && regex_match(m.text, anyNumber) /*&& PlCall("writeln('OK')")*/){
				if (debug_enabled())
					PlCall("writeln", PlTerm(m.text.c_str()));
				match=m;
			}
		//waitKey(1000);//////////////////////////////////////////
	}
	//match.text.
#ifdef _DEBUG
	if (debug_enabled()){
		Mat img2;
		cvtColor(img, img2, CV_GRAY2BGR);
		cout<<match.text<<": "<<match.probability<<"%\n";
		rectangle(img2, match.bounds, Scalar(255, 0, 0));
		putText(img2, match.text+"("+to_string((int)(match.fontSize*10))+")", match.bounds.tl(), FONT_HERSHEY_COMPLEX_SMALL, .6, Scalar(0));
		imshow("Image", img);
		imshow("Contour", img2);
		waitKey(1);
	}
#endif // _DEBUG*/
	PlTerm poly;
	PlTail lv(poly);
	Rect r=match.bounds;
	lv.append(P(r.x, r.y));
	lv.append(P(r.x, r.y+r.height));
	lv.append(P(r.x+r.width, r.y+r.height));
	lv.append(P(r.x+r.width, r.y));
	lv.close();
	return (A2=PlString(match.text.c_str()))&&(A3=PlCompound("poly", PlTermv(PlTerm((long)4), poly)))&&(A4=PlTerm((long)(match.fontSize*10)));
}
PREDICATE(read_text, 4){	//read_text(+Img,-TextList,-RectList,-FontSizeList)
	if (strcmp(A1.name(), "img")||A1.arity()!=1)
		return FALSE;
	Mat img, *input=(Mat*)(void*)A1[1];	//PlTerm args index starts from 1
	if (input->channels()>1)
		cvtColor(*input, img, COLOR_BGR2GRAY);
	else
		img=*input;
	vector<TextMatch> matches;
	findTextMultiscale(img, matches, 0.5f, 2);
	PlTerm rt, rr, rs;
	PlTail lt(rt), lr(rr), ls(rs);
	for (TextMatch &m : matches){
		lt.append(PlString(m.text.c_str()));
		PlTerm poly;
		PlTail lv(poly);
		Rect r=m.bounds;
		lv.append(P(r.x, r.y));
		lv.append(P(r.x, r.y+r.height));
		lv.append(P(r.x+r.width, r.y+r.height));
		lv.append(P(r.x+r.width, r.y));
		lv.close();
		lr.append(PlCompound("poly", PlTermv(PlTerm((long)4), poly)));
		ls.append(PlTerm((long)(m.fontSize*10)));
	}
	lt.close();
	lr.close();
	ls.close();
	return (A2=rt) && (A3=rr) && (A4=rs);
}
PREDICATE(write_text, 3){	//write_text(+Img,+Text,+Poly)
	if (strcmp(A1.name(), "img")|| A1.arity()!=1)
		return FALSE;
	if (strcmp(A3.name(), "poly")|| A3.arity()!=2)
		return FALSE;
	Mat* img=(Mat*)(void*)A1[1];
	vector<Point> v;
	polyToVertexVector(A3, v);
	Rect r=boundingRect(v);
	int h=r.height/2;
	putText(*img, string(A2), Point(r.x+r.width/2-h*7/12, r.y+(r.height+h)/2), FONT_HERSHEY_TRIPLEX, h/20.0, Scalar(0));
	return TRUE;
}
