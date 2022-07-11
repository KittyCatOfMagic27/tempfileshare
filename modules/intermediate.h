#ifndef INTER_H
#define INTER_H

#include <vector>
#include <string>
#include <cstdio>
#include <cassert>
#include <iostream>
#include "lexer.h"

using namespace std;

namespace intr{
  string size_of(lex::ENUM_TYPE type){
    if(type == lex::UINT) return "i32";
    if(type == lex::INT) return "i32";
    if(type == lex::LONG) return "i64";
    if(type == lex::PTR){
      if(options::target==options::X86_I386) return "i32";
      return "i64";
    }
    if(type == lex::BOOL) return "i8";
    if(type == lex::CHAR) return "i8";
    return "UKNOWN_SIZE";
  }
  
  struct fn_node{
    fn_node(string _label) : label(_label){}
    string label;
    vector<string> children;
  };
  
  class INTERMEDIATE{
    public:
    
    vector<lex::Token> Tokens;
    vector<string> INTER;
    
    INTERMEDIATE(vector <lex::Token> &_Tokens){
      Tokens = _Tokens;
    }
    
    vector<string> run(){
      tks_to_inter();
      deadcode_elim();
      grammer_check();
      return INTER;
    }
    
    private:
    
    void setup_op(lex::Token &tk1, lex::Token &tk2){
      INTER.push_back(size_of(tk1.type));
      INTER.push_back(size_of(tk2.type));
      if(lex::literal_or_var(tk1.value)!=lex::ERROR) INTER.push_back(tk1.value);
      else INTER.push_back("?"+tk1.value);
      if(lex::literal_or_var(tk2.value)!=lex::ERROR) INTER.push_back(tk2.value);
      else INTER.push_back("?"+tk2.value);
    }
    void tks_to_inter(){
      printf("Generate intermediate representation...\n");
      lex::Token tk1;
      lex::Token tk2;
      string prev;
      if(options::DEBUGMODE){
        for(lex::Token &tk : Tokens){
          cout << lex::TYPE_NAMES[tk.type] << " ";
          if(tk.value!=""){
            cout << tk.value << " ";
          }
          if(tk.type==lex::SEMICOLON){
            cout << "\n";
          }
        }
      }
      for(int i = 0; i<Tokens.size(); i++){
        lex::Token tk = Tokens[i];
        switch(tk.type){
          case lex::PROC:
          INTER.push_back("proc");
          INTER.push_back(tk.value);
          INTER.push_back(";");
          break;
          case lex::END:
          INTER.push_back("end");
          INTER.push_back(tk.value);
          INTER.push_back(";");
          break;
          case lex::BEGIN:
          INTER.push_back("begin");
          break;
          case lex::IN:
          INTER.push_back("in");
          break;
          case lex::OUT:
          INTER.push_back("out");
          break;
          case lex::PUSH:
          INTER.push_back("push");
          INTER.push_back(size_of(Tokens[i+1].type));
          if(lex::literal_or_var(Tokens[i+1].value)==lex::ERROR) INTER.push_back("?"+Tokens[i+1].value);
          else INTER.push_back(Tokens[i+1].value);
          INTER.push_back(";");
          i++;
          break;
          case lex::POP:
          INTER.push_back("pop");
          INTER.push_back(size_of(Tokens[i+1].type));
          if(lex::literal_or_var(Tokens[i+1].value)==lex::ERROR) INTER.push_back("?"+Tokens[i+1].value);
          else INTER.push_back(Tokens[i+1].value);
          INTER.push_back(";");
          i++;
          break;
          case lex::SYSCALL:
          INTER.push_back("syscall");
          break;
          case lex::OPARA:
          INTER.push_back("(");
          break;
          case lex::CPARA:
          INTER.push_back(")");
          break;
          case lex::CALL:
          INTER.push_back("call");
          INTER.push_back(tk.value);
          break;
          case lex::UINT:
          case lex::INT:
          case lex::LONG:
          case lex::PTR:
          case lex::BOOL:
          case lex::CHAR:
          case lex::STRING:
          if(lex::literal_or_var(tk.value)==lex::ERROR) INTER.push_back("?"+tk.value);
          else INTER.push_back(tk.value);
          break;
          case lex::DEFS:
          INTER.push_back("def");
          if(Tokens[i+1].type!=lex::OSQRB){
            INTER.push_back("i"+to_string((Tokens[i+1].value.size()-2)*8));
            INTER.push_back("?"+tk.value);
            if(Tokens[i+1].type>=lex::UINT&&Tokens[i+1].type<=lex::AMPERSAND) INTER.push_back("=");
            else if(Tokens[i+1].type==lex::OSQRB) continue;
            else INTER.push_back(";");
          }
          else{
            i++;
            INTER.push_back("i"+to_string(stoi(Tokens[++i].value)*8));
            i++;
            INTER.push_back("?"+tk.value);
            if(Tokens[i+1].type>=lex::UINT&&Tokens[i+1].type<=lex::AMPERSAND) INTER.push_back("=");
            else INTER.push_back(";");
          }
          break;
          case lex::DEFU:
          case lex::DEFI:
          INTER.push_back("def");
          INTER.push_back("i32");
          INTER.push_back("?"+tk.value);
          if(Tokens[i+1].type>=lex::UINT&&Tokens[i+1].type<=lex::AMPERSAND) INTER.push_back("=");
          else if(Tokens[i+1].type==lex::OSQRB) continue;
          else INTER.push_back(";");
          break;
          case lex::DEFP:
          case lex::DEFL:
          INTER.push_back("def");
          if(options::target==options::X86_I386) INTER.push_back("i32");
          else INTER.push_back("i64");
          INTER.push_back("?"+tk.value);
          if(Tokens[i+1].type>=lex::UINT&&Tokens[i+1].type<=lex::AMPERSAND) INTER.push_back("=");
          else if(Tokens[i+1].type==lex::OSQRB) continue;
          else INTER.push_back(";");
          break;
          case lex::DEFC:
          case lex::DEFB:
          INTER.push_back("def");
          INTER.push_back("i8");
          INTER.push_back("?"+tk.value);
          if(Tokens[i+1].type>=lex::UINT&&Tokens[i+1].type<=lex::AMPERSAND) INTER.push_back("=");
          else if(Tokens[i+1].type==lex::OSQRB) continue;
          else INTER.push_back(";");
          break;
          case lex::ADD:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("+");
          setup_op(tk1, tk2);
          break;
          case lex::SUB:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("-");
          setup_op(tk1, tk2);
          break;
          case lex::MULT:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("*");
          setup_op(tk1, tk2);
          break;
          case lex::DIV:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("/");
          setup_op(tk1, tk2);
          break;
          case lex::MOD:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("%");
          setup_op(tk1, tk2);
          break;
          case lex::ECMP:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("==");
          setup_op(tk1, tk2);
          break;
          case lex::LCMP:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("<");
          setup_op(tk1, tk2);
          break;
          case lex::GCMP:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back(">");
          setup_op(tk1, tk2);
          break;
          case lex::GECMP:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back(">=");
          setup_op(tk1, tk2);
          break;
          case lex::LECMP:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("<=");
          setup_op(tk1, tk2);
          break;
          case lex::NECMP:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("!=");
          setup_op(tk1, tk2);
          break;
          case lex::AND:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("&&");
          setup_op(tk1, tk2);
          break;
          case lex::BIT_AND:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("-&");
          setup_op(tk1, tk2);
          break;
          case lex::SHR:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back(">>");
          setup_op(tk1, tk2);
          break;
          case lex::SHL:
          tk1 = Tokens[++i];
          tk2 = Tokens[++i];
          INTER.push_back("<<");
          setup_op(tk1, tk2);
          break;
          case lex::AMPERSAND:
          tk1 = Tokens[++i];
          INTER.push_back("&");
          INTER.push_back("?"+tk1.value);
          break;
          case lex::DEREF:
          tk1 = Tokens[++i];
          INTER.push_back("@");
          INTER.push_back("?"+tk1.value);
          break;
          case lex::INC:
          prev = INTER[INTER.size()-1];
          INTER[INTER.size()-1] = "++";
          INTER.push_back(prev);
          break;
          case lex::DEC:
          prev = INTER[INTER.size()-1];
          INTER[INTER.size()-1] = "--";
          INTER.push_back(prev);
          break;
          case lex::SEMICOLON:
          if(INTER[INTER.size()-1]!=";") INTER.push_back(";");
          break;
          case lex::OSQRB:
          INTER.push_back("[");
          break;
          case lex::CSQRB:
          INTER.push_back("]");
          break;
          case lex::STATIC:
          INTER.push_back("static");
          break;
          case lex::EXTERN:
          INTER.push_back("extern");
          break;
          case lex::WHILE:
          INTER.push_back("while");
          break;
          case lex::IF:
          INTER.push_back("if");
          break;
          default:
          cerr << "\033[1;31m"<<lex::FILENAME<<":"<<tk.line<<":Unhandled token type in intermediate '"<<lex::TYPE_NAMES[tk.type]<<"'.\033[0m\n";
          assert(false);
          break;
        }
      }
      printf("Done generate intermediate representation!\n");
    }
    void parse_tree(string &current, unordered_map<string, fn_node*> &tree, vector<string> &list){
      cout << "CURRENT: "<<current<<"\n";
      cout << "CHILD AMOUNT: " << tree[current]->children.size() << "\n";
      for(string &s : tree[current]->children){
        list.push_back(s);
        cout << "    CHILD: "<<s<<"\n";
        if(tree[s]->children.size()>0){
          parse_tree(s, tree, list);
        }
      }
    }
    void deadcode_elim(){
      unordered_map<string, int> fns;
      unordered_map<string, fn_node*> tree;
      vector<fn_node> storage;
      fn_node* CP;
      for(int i = 0; i < INTER.size(); i++){
        if(INTER[i]=="proc" && INTER[i+1]!="main"){
          fn_node proc     = fn_node(INTER[i+1]);
          storage.push_back(proc);
          tree[INTER[i+1]] = &storage[storage.size()-1];
          CP               = &storage[storage.size()-1];
          fns[INTER[++i]]  = i-1;
          i+=2;
        }
        else if(INTER[i]=="proc" && INTER[i+1]=="main"){
          fn_node proc     = fn_node(INTER[i+1]);
          storage.push_back(proc);
          tree[INTER[i+1]] = &storage[storage.size()-1];
          CP               = &storage[storage.size()-1];
          i++;
        }
        else if(INTER[i]=="call" && fns.find(INTER[i+1]) != fns.end()){
          vector<string>* children = &CP->children;
          if(find(children->begin(), children->end(), INTER[i+1])==children->end()) children->push_back(INTER[i+1]);
          i+=2;
        }
      }
      vector<string> list;
      string main = "main";
      list.push_back(main);
      parse_tree(main, tree, list);
      //First clean up
      storage.clear();
      tree.clear();
      
      cout << "LIST: {";
      for(string &s : list){
        cout << s << ", ";
      }
      cout << "}\n";
      for(string &s : list){
        fns.erase(s);
      }
      cout << "PROCS TO ERASE: {\n";
      for(auto &p : fns){
        cout << "    {"<<p.first<<", "<<p.second<<"},\n";
      }
      cout << "}\n";
      int offset = 0;
      for(auto &p : fns){
        int i = p.second-offset;
        int begin = i;
        cout << i << "\n";
        cout << INTER[begin] << "\n";
        while(i<INTER.size()){
          if(INTER[i]=="end"&&INTER[i+1]==p.first){
            i+=2;
            break;
          }
          i++;
        }
        INTER.erase(INTER.begin()+begin,INTER.begin()+i);
        offset += begin-i;
      }
      //Second clean up
      list.clear();
      fns.clear();
    }
    void grammer_check(){
      int WHILE_COUNT = 0;
      int IF_COUNT    = 0;
      for(int i = 0; i < INTER.size()-1; i++){
        string s = INTER[i];
        if(s==";"&&INTER[i+1][0]=='?'){
          INTER.insert(INTER.begin()+i+2, "=");
          i++;
        }
        else if(s=="while"){
          //Erase and get info
          int beginning = i;
          if(INTER[++i]!="("){
            cerr << "\033[1;31mNo open paren with while dec.\033[0m\n";
            assert(false);
          }
          vector<string> expression;
          while(INTER[++i]!=")") expression.push_back(INTER[i]);
          i++;
          INTER.erase(INTER.begin()+beginning,INTER.begin()+i+1);
          i = beginning;
          //Set up the beginning of the while loop
          vector<string> insertv;
          insertv.push_back("jmp");
          insertv.push_back(".whilecmp"+to_string(++WHILE_COUNT));
          insertv.push_back(";");
          insertv.push_back("label");
          insertv.push_back(".while"+to_string(WHILE_COUNT));
          insertv.push_back(";");
          INTER.insert(INTER.begin()+i,insertv.begin(), insertv.end());
          i+=insertv.size()-1;
          //Find End
          //while(INTER[++i]!="end"&&INTER[i+1]!="while");
          int bc = 1;
          while(true){
            if(INTER[++i]=="while") bc++;
            else if(INTER[i]=="end"&&INTER[i+1]=="while"){
              if(--bc==0) break;
              i++;
            }
          }
          INTER.erase(INTER.begin()+i,INTER.begin()+i+3);
          //Setup end
          insertv.clear();
          insertv.push_back("label");
          insertv.push_back(".whilecmp"+to_string(WHILE_COUNT));
          insertv.push_back(";");
          insertv.push_back("jmpc");
          insertv.push_back(".while"+to_string(WHILE_COUNT));
          insertv.push_back(expression[0]);
          insertv.push_back(";");
          INTER.insert(INTER.begin()+i,insertv.begin(), insertv.end());
          i = beginning+1;
        }
        else if(s=="if"){
          //Erase and get info
          int beginning = i;
          if(INTER[++i]!="("){
            cerr << "\033[1;31mNo open paren with if dec.\033[0m\n";
            assert(false);
          }
          vector<string> expression;
          while(INTER[++i]!=")") expression.push_back(INTER[i]);
          i++;
          INTER.erase(INTER.begin()+beginning,INTER.begin()+i+1);
          i = beginning;
          //Set up the beginning of the if
          vector<string> insertv;
          insertv.push_back("jmpc");
          insertv.push_back(".if"+to_string(++IF_COUNT));
          insertv.push_back(expression[0]);
          insertv.push_back("!");
          insertv.push_back(";");
          INTER.insert(INTER.begin()+i,insertv.begin(), insertv.end());
          i+=insertv.size()-1;
          //Find End
          //while(INTER[++i]!="end"&&INTER[i+1]!="while");
          int bc = 1;
          while(true){
            if(INTER[++i]=="if") bc++;
            else if(INTER[i]=="end"&&INTER[i+1]=="if"){
              if(--bc==0) break;
              i++;
            }
          }
          INTER.erase(INTER.begin()+i,INTER.begin()+i+3);
          //Setup end
          insertv.clear();
          insertv.push_back("label");
          insertv.push_back(".if"+to_string(IF_COUNT));
          insertv.push_back(";");
          INTER.insert(INTER.begin()+i,insertv.begin(), insertv.end());
          i = beginning+1;
        }
      }
    }
  };
  
}

#endif
