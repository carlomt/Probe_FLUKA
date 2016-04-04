#include <iostream>
#include <fstream>
#include <map>
                
#include <stdlib.h>
#include <math.h>
#include <sstream>

#include <time.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "TROOT.h"
#include <TApplication.h>
#include "TSystem.h"
#include "TStyle.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TH1D.h"
#include "TCanvas.h"
#include "TTree.h"
#include "TNtuple.h"
#include "TLegend.h"
#include "TFile.h"
#include "TCut.h"
#include "TCutG.h"
#include "TMath.h"

using namespace std;

int FileConverter(string filename,int mesglevel,int version,string outfname);
int FileConverter(vector<string> filenames,int mesglevel,int version,string outfname);
void UnreadColumnsChecker(istringstream &iss, int counter);
void print_help(string fname="executable");
string getFileCreationTime(string path);
string CheckOutputExtension(const string fname);

int main(int argc, char *argv[])
{
  cout<<argv[0]<< " 2.2"<<endl;
  cout<<"Last edit:   Jan 28 2015."<<endl;
  cout<<"Compiled at: "<< __DATE__ <<", "<< __TIME__<<"."<<endl;
  
  string execname=argv[0];
  // execname.pop_back();
  // string sourcename=execname+"C";
  std::string sourcename=execname.substr(0,execname.size()-1)+"C";
  cout<<"from source file: "<<sourcename<<endl;
  cout<<"edited last time at: "<<getFileCreationTime(sourcename)<<endl;
  
  int success=-1;
  int result=1;
  int mesglevel=0;
  int version=1;
  string outfname="";
  vector<string> filenames;

  if(argc==1)
    {
      print_help(argv[0]);
      exit(0); 
    }
  else
    {
      for(int i=1;i<argc;i++)
	if(argv[i][0] =='-')
	  {
	    string option(argv[i]);
	    cout<<"option: "<<i<<" "<<option<<endl;
	    if(option.compare("-help")==0)
	      {
		print_help(argv[0]);
		exit(0); 
	      }
	    else if(option.compare("-verbose")==0)
	      {
		mesglevel=1; 
	      }
	    else if(option.compare("-v")==0)
	      {
		version=atoi(argv[++i]);
	      }
	    else if(option.compare("-o")==0)
	      {
		outfname=argv[++i];
	      }
	    else
	      {
		cout<<"option not recognized: "<<argv[i];
		exit(-1);
	      }
	  }
	else
	  {
	    filenames.push_back(argv[i]);
	    cout<<"adding file: "<<argv[i]<<" to the input file list."<<endl;
	  }
    }

  
  success= FileConverter(filenames,mesglevel,version,outfname);
  if(success<0)
    {
      result=-1;
    }
  return result;
}

int FileConverter(string filename, int mesglevel,int version,string outfname)
{
  vector<string> filenames;
  filenames.push_back(filename);
  return FileConverter(filenames,mesglevel,version,outfname);
}

int FileConverter(vector<string> filenames, int mesglevel,int version,string outfnameI)
{
  cout<<"input file version: "<<version<<endl;
  if(outfnameI.compare("")==0)
    {
      outfnameI=filenames[0];
    }
  const string outfname=CheckOutputExtension(outfnameI);
  cout<<"output file name: "<<outfname<<endl;

  TFile *hfile = new TFile(outfname.c_str(),"RECREATE","Example");

  TTree *DataTree = new TTree("EventTree","FLUKA mc data");

  //nPrim/I:npmt/I:nnPart/I:idPrReal/I:EkPrim/D:pidPrim/I:EkOut/D:EpteTot/D:EptePh/D:EpteCh/D:XPrim/D:YPrim/D:ZPrim/D:CXPrim/D:CYPrim/D:CZPrim/D:Pprim/D:Mprim/D

  // Declaration of data types for the DataTree
  //  Int_t nPrim;
  Int_t    nPrim; 
  Int_t    npmt; 
  Int_t    nInPart; 
  Int_t    idPrReal; 
  Double_t EkPrim; 
  Int_t    pidPrim; 
  Double_t EkOut; 
  Double_t EpteTot; 
  Double_t EptePh; 
  Double_t EpteCh; 
  Double_t XPrim; 
  Double_t YPrim; 
  Double_t ZPrim; 
  Double_t CXPrim; 
  Double_t CYPrim; 
  Double_t CZPrim; 
  Double_t Pprim; 
  Double_t Mprim;   
  Double_t EprimPte;
  Int_t RegPrim;
  Int_t    nInPartS; 
  Double_t EpteTotS; 
  Double_t EptePhS; 
  Double_t EpteChS; 

  DataTree->Branch("nPrim",    &nPrim,  "nPrim/I");     
  DataTree->Branch("RegPrim",    &RegPrim,  "RegPrim/I");     
  DataTree->Branch("npmt",     &npmt,   "npmt/I");       
  DataTree->Branch("nInPart",   &nInPart, "nInPart/I");   
  DataTree->Branch("idPrReal", &idPrReal,"idPrReal/I");
  DataTree->Branch("EkPrim",   &EkPrim, "EkPrim/D");   
  DataTree->Branch("pidPrim",  &pidPrim,"pidPrim/I"); 
  DataTree->Branch("EkOut",    &EkOut,  "EkOut/D");     
  DataTree->Branch("EpteTot",  &EpteTot,"EpteTot/D"); 
  DataTree->Branch("EptePh",   &EptePh, "EptePh/D");   
  DataTree->Branch("EpteCh",   &EpteCh, "EpteCh/D");   
  DataTree->Branch("XPrim",    &XPrim,  "XPrim/D");     
  DataTree->Branch("YPrim",    &YPrim,  "YPrim/D");     
  DataTree->Branch("ZPrim",    &ZPrim,  "ZPrim/D");     
  DataTree->Branch("CXPrim",   &CXPrim, "CXPrim/D");   
  DataTree->Branch("CYPrim",   &CYPrim, "CYPrim/D");   
  DataTree->Branch("CZPrim",   &CZPrim, "CZPrim/D");   
  DataTree->Branch("Pprim",    &Pprim,  "Pprim/D");     
  DataTree->Branch("Mprim",    &Mprim,  "Mprim/D");     
  DataTree->Branch("EprimPte",    &EprimPte,  "EprimPte/D");     
  DataTree->Branch("nInPartS",   &nInPartS, "nInPartS/I");   
  DataTree->Branch("EpteTotS",  &EpteTotS,"EpteTotS/D"); 
  DataTree->Branch("EptePhS",   &EptePhS, "EptePhS/D");   
  DataTree->Branch("EpteChS",   &EpteChS, "EpteChS/D");   

  for(size_t iFile=0; iFile<filenames.size(); iFile++)
    {
      const string originalfilename=filenames[iFile];
      cout<<"Converting file: "<<originalfilename<<endl;
      string filename=filenames[iFile];
      size_t pos = filename.find(".");
      string filename_ext = filename.erase(0, pos);

      ifstream freader;
      if(filename_ext.compare(".38")!=0 )
	{
	  cout<<"WARNING: this program is made to read output 38"<<endl;
	}
      freader.open(originalfilename.c_str());
      
      size_t datasize=0;
      if(version==2)
	{
	  datasize=18;
	}
      else if(version==1)
	{
	  datasize=16;
	}
      else if(version==3)
	{
	  datasize=19;
	}
      else if(version==4)
	{
	  datasize=24;
	}
      else
	{
	  cout<<"ERROR: version "<<version<<" not defined."<<endl;
	}

      string command="wc -l "+originalfilename;
      char buff[512];
      FILE* fpipe = popen(command.c_str(), "r");
      fgets(buff, sizeof(buff),fpipe);
      pclose(fpipe);
      istringstream iscrr(buff);
      int totline=-1;
      iscrr >> totline;
      cout<<"total number of lines: "<<totline<<endl;

      string line;
      if(freader.is_open())
	{ 
	  int counter=0;
	  float progress = 0.0;
	  int barWidth = 70;
	  while(!freader.eof())
	    {
	      vector<double> appvec;
	      double app=-99.;
	      
	      while(appvec.size() < datasize )
		{
		  getline(freader,line);
		  counter++;

		  /*
		    if(counter%1000000==0)
		    cout<<counter<<" lines read so far..."<<endl;
		  */
		  if(mesglevel==0) 
		    {
		      progress=1.*counter/totline;
		      std::cout << "[";
		      int pos = (int)(barWidth * progress);
		      for (int i = 0; i < barWidth; ++i) 
			{
			  if (i < pos) std::cout << "=";
			  else if (i == pos) std::cout << ">";
			  else std::cout << " ";
			}
		      std::cout << "] " << int(progress * 100.0) << " %\r";
		      std::cout.flush();
		    }
		  istringstream iss(line);
		  if(mesglevel>0) 
		    {
		      cout<<"line "<<counter<<" "<<line<<endl;
		    }
		  while(iss.good())
		    {
		      iss >> app;
		      appvec.push_back(app);
		    }
		  UnreadColumnsChecker(iss,counter);

		}
	      if(mesglevel>0) 
		{
		  cout<<appvec.size()<<" numbers read"<<endl;
		}

	      if(appvec.size()!=datasize)
		{
		  cout<<"ERROR: the program has read "<<appvec.size()<<" columns from the lines "<<counter-1<<" and "<<counter<<" while "<<datasize<<" were expected (as you required the version "<<version<<" .38 format file)."<<endl;
		  return(-1);
		}
	      if(mesglevel>0) 
		{
		  cout<<"writing: ";
		  for(size_t ii=0;ii<appvec.size();ii++)
		    {
		      cout<<appvec[ii]<<" ";
		    }
		  cout<<endl;
		  cout.flush();
		}
     //     WRITE(38,*) nPrim,nPhot,nInPart
	      nPrim = (int) appvec[0]; 
	      npmt = (int) appvec[1]; 
	      nInPart = (int) appvec[2]; 
     // &        , nPrimRe, rEprim, nPidPrim, rEOutSou
	      idPrReal = (int) appvec[3]; 
	      EkPrim = appvec[4]; 
	      pidPrim = (int) appvec[5]; 
	      EkOut = appvec[6]; 
     // &        , rEInPte, rEInPtePh, rEInPteCh
	      EpteTot = appvec[7]; 
	      EptePh = appvec[8]; 
	      EpteCh = appvec[9]; 
     // &        , rXprim, rYprim, rZprim
	      XPrim = appvec[10]; 
	      YPrim = appvec[11]; 
	      ZPrim = appvec[12]; 
     // &        , rCXprim, rCYprim, rCZprim
	      CXPrim = appvec[13]; 
	      CYPrim = appvec[14]; 
	      CZPrim = appvec[15]; 
     // &        , rPprim, rMprim, rEprimPte
	      Pprim = -99.; 
	      Mprim = -99.;  
	      EprimPte = -99.;
     // &        , nRegPrim
	      RegPrim = -99;
     // &        , nInPart
	      nInPartS = -99; 
     // &        , rEInPteS, rEInPtePhS, rEInPteChS
	      EpteTotS = -99.; 
	      EptePhS = -99.; 
	      EpteChS = -99.; 

	      if(version>1)
		{
		  Pprim = appvec[16]; 
		  Mprim = appvec[17];  
		}
	      if(version>2)
		{
		  EprimPte = appvec[18];
		}
	      if(version>3)
		{
		  RegPrim = appvec[19];
		  nInPartS = appvec[20];
		  EpteTotS = appvec[21];
		  EptePhS = appvec[22]; 
		  EpteChS = appvec[23]; 

		}
	      if(nPrim!=-99)
		{
		  if(mesglevel>0) 
		    {
		      cout<<"Filling"<<endl<<endl;
		    }
		  DataTree->Fill();
		}
	    }
	  std::cout << std::endl;
  
	}      
      else
	{
	  cout<<"Unable to open file"<<endl;
	  return(-1);
	}
    }
  DataTree->Write();
  hfile->Write();
  hfile->Close();
  return(1);
  
}



void print_help(string fname)
{
  cout<<"Source: "<<__FILE__<<endl;
  cout<<endl;
  cout<<"Usage  : "<<fname<<" (option) <evtfile>"<<endl;
  cout<<"Option : -verbose  (show debug output)"<<endl;
  cout<<"Option : -o  (set output filename, by default is the name of the first input)"<<endl;
  cout<<"Option : -v  (version of the file 38: "<<endl
      <<"\t 1) for the old simulation;"<<endl
      <<"\t 2) for the output with mass and momentum of primaries;"<<endl
      <<"\t 3) reads also the primaries energy entering the probe; "<<endl
      <<"\t 4) with flag for the creation region)"<<endl;
  cout<<"Option : -help     (show this help)"<<endl;
  //    printf("       : -log (Log filename)\n"); 
  cout<<endl;

  return;
}

void UnreadColumnsChecker(istringstream &iss, int counter)
{
  int unread_columns=0;
  int last_unread;
  while(iss.good())
    {
      iss>>last_unread;
      unread_columns++;
    }
  unread_columns--;
  if(unread_columns>0)
    {
      cout<<"Warning: in the line "<<counter<<" there are "<<unread_columns<<" more columns that the program didn't read. Check the code."<<endl;
      cout<<"The last line number in this line is: "<<last_unread<<endl;
    }
}

string getFileCreationTime(string path) {
    struct stat attr;
    stat(path.c_str(), &attr);
    return ctime(&attr.st_mtime);
}


string CheckOutputExtension(const string fname)
{
  string OutFile;
  string s = fname;
  size_t pos = 0;
  string token;
  bool atleastone=false;
  const string delimiter=".";
  while ((pos = s.find(delimiter)) != std::string::npos) 
    {
      token = s.substr(0, pos);
      //      std::cout << token << std::endl;
      s.erase(0, pos + delimiter.length());
      atleastone=true;
    }
  if(s.compare("root")==0)
    {
      OutFile=fname;
    }
  else
    {
      //      std::cout<<"Warning: you passed a file with extension "<<s<<", I will remove it and use .root "<<std::endl;
      if(atleastone)
	{
	  OutFile=token+".root";
	}
      else
	{
	  OutFile=fname+".root";
	}
    }
  return OutFile;
}
