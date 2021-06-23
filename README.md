
# Parsing AODocs JSON files using Haskell 


## Purpose 

These modules are a simple example of using Haskell to parse JSON files, and export the information into CSVs. 

## Background 

AODocs is a document management platform tightly integrated with Google Drive, providing all the features G Suite customers need to implement their business workflows, control their documents or apply retention policies.  

One of their object types is called Document CLass or DocClass. The JSON specifications for these can be exported via:
https://apis-explorer.appspot.com/apis-explorer/?base=https://ao-docs.appspot.com/_ah/api#s/ 

The JSON parser was built in Haskell 8 using the aeson and cassava packages. This software has only been used by myself, for automating aspects of design. It was only run intermittently when a new design task was at hand. Therefore, the IO is very simple, consisting of CSV file handing. There is also no Cabal install.  

Any errors or misapplication of Haskell are my own. 

## Design - Custom Modules 

The following includes hs modules, as well as input json files, and output csv files. 

File	| 	Ord	| 	Lines	| 	FT	| 	For
-----------------------	| 	--	| 	-----	| 	----	| 	-----------------------
AODocsEnum.hs	| 	1	| 	180	| 	hs	| 	Sum types needed for AODocs JSON. These are across many AODocs JSON files. 
AODocsCommon.hs	| 	2	| 	160	| 	hs	| 	shared common types needed for AODocs JSON. These are across many AODocs JSON files. 
CassavaUtils.hs	| 	3	| 	60	| 	hs	| 	Enhances  Data.csv (Cassava) slightly with catchShowIO and FromField and ToField for Bool. Both may need to be changed for different JSON libraries. 
AODocsDocClass.hs	| 	4	| 	480	| 	hs	| 	For the AODocs Document Class JSON type, this module defines the Type and Eq and Ord instances. Using Data.Aeson it defines instances for FromJSON and ToJSON for parsing. Using Data.csv (Cassava), it defines  FromNamedRecord, ToNamedRecord and DefaultOrdered instances for csv export. 
AODocsLibrary.hs	| 	5	| 	560	| 	hs	| 	For the AODocs Library Class JSON type, this module defines the Type and Eq and Ord instances. Using Data.Aeson it defines instances for FromJSON and ToJSON for parsing. Using Data.csv (Cassava), it defines  FromNamedRecord, ToNamedRecord and DefaultOrdered instances for csv export. 
AODocsView.hs	| 	6	| 	270	| 	hs	| 	For the AODocs View Class JSON type, this module defines the Type and Eq and Ord instances. Using Data.Aeson it defines instances for FromJSON and ToJSON for parsing. Using Data.csv (Cassava), it defines  FromNamedRecord, ToNamedRecord and DefaultOrdered instances for csv export. 
AODocsWorkflow.hs	| 	7	| 	300	| 	hs	| 	For the AODocs Workflow Class JSON type, this module defines the Type and Eq and Ord instances. Using Data.Aeson it defines instances for FromJSON and ToJSON for parsing. Using Data.csv (Cassava), it defines  FromNamedRecord, ToNamedRecord and DefaultOrdered instances for csv export. 
AODocs.Main.DocClass.hs	| 	8	| 	170	| 	hs	| 	For the AODocs Document Class JSON type, this program parses the JSON and writes out csv files. 
AODocs.Main.Library.hs	| 	9	| 	110	| 	hs	| 	For the AODocs Library Class JSON type, this program parses the JSON and writes out csv files. 
AODocs.Main.View.hs	| 	10	| 	80	| 	hs	| 	For the AODocs View Class JSON type, this program parses the JSON and writes out csv files. 
AODocs.Main.Workflow.hs	| 	11	| 	100	| 	hs	| 	For the AODocs Workflow Class JSON type, this program parses the JSON and writes out csv files. 
0 DocClass.json	| 	12	| 	14,800	| 	json	| 	Example of an AODocs Document Class JSON file. 
0 Libraries.json	| 	13	| 	32,500	| 	json	| 	Example of an AODocs Library Class JSON file. 
AM2.views.json	| 	14	| 	33,300	| 	json	| 	Example of an AODocs View Class JSON file. 
0 Workflows.json	| 	15	| 	13,700	| 	json	| 	Example of an AODocs Workflow Class JSON file. 
alldocClassPermsCompare.csv	| 	16	| 	18	| 	csv	| 	Example of an AODocs Document Class csv output file. 
libout.csv	| 	17	| 	82	| 	csv	| 	Example of an AODocs Library Class csv output file. 
vf1.csv	| 	18	| 	36	| 	csv	| 	Example of an AODocs View Class csv output file. 
workflowPerms.csv	| 	19	| 	149	| 	csv	| 	Example of an AODocs Workflow Class csv output file. 
AODocs.Main.CSVDiff.hs	| 	20	| 	170	| 	hs	| 	Conducts simple count and key tests 
AODocs.Main.Test.hs	| 	21	| 	50	| 	hs	| 	Incomplete. Possible proposition tests, but ran out of time to complete. 


