#define _CRT_SECURE_NO_WARNINGS
#include <opencv2/opencv.hpp>
#include <string.h>
//#include <windows.h>
//#include <SDL2/SDL.h> 
//#include <SDL2/SDL_timer.h> 
#include <semaphore.h>
using namespace cv;
using namespace std;


void window(const char* n){
    namedWindow("Image", WINDOW_AUTOSIZE);
}

/*
DWORD WINAPI _window(void* n){
	namedWindow(string((char*)n));
    ReleaseSemaphore(s, 1, NULL);
	while (true) waitKey();
}

void window(const char* n){
	CreateThread(NULL, 0, _window, (void*)n, NULL, NULL);
	WaitForSingleObject(s, INFINITE);
}


void alert(const char* m){
#ifdef UNICODE
	size_t l=strlen(m)+1;
	wchar_t* wm=(wchar_t*)malloc(l*sizeof(wchar_t));
	mbstowcs(wm, m, l);
	MessageBox(NULL, wm, L"", MB_ICONINFORMATION);
	free(wm);
#else
	MessageBox(NULL, m, "", MB_ICONINFORMATION);
#endif
}
*/
