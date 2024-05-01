//--------------core------------
#include<vector>
#include<map>
#include<string>
#include<iostream>
#include<sstream>
#include<cstdlib>
using namespace std;

namespace pl{

	class value;


	class symbol_map{
	public:
		value* operator[](string name){
			map<string, value*>::iterator r= map_sv.find(name);
			if(r==map_sv.end()) return 0;
			return r->second;
		}
		string operator[](value* val){
			map<value*,string>::iterator r= map_vs.find(val);
			if(r==map_vs.end()) return "nameless";
			return r->second;
		}
		void bind(string name, value* val);
		map<string,value*> map_sv;
		map<value*,string> map_vs;
	}glb_symbol_map;

	class env_tree;
	class value{
	public:
		class gc{
		public:
			gc(){count=10;}
			value* push(value* v){
				v->auto_del=false;
				pool.push_back(v);
				if(--count==0) collect();
				return v;
			}
			value* uncollect(value* v){
				v->auto_del=false;
				return v;
			}
			value* auto_collect(value* v){
				v->auto_del=true;
				return v;
			}
			void collect(){
				//mark false
				for(size_t i=0; i<pool.size(); ++i){
					pool[i]->gc_state= false;
				}
				//mark static flag
				for(size_t i=0; i<pool.size(); ++i){
					if(pool[i]->auto_del==false)pool[i]->gc_set_state(true);
				}
				//temp vector
				vector<value*> temp;
				//collect
				for(size_t i=0; i<pool.size(); ++i){
					if(pool[i]->gc_state==true){
						temp.push_back(pool[i]);
					}else{
						delete pool[i];
					}
				}
				count= temp.size();
				pool= move(temp);
			}
			vector<value*> pool;
			size_t count;
		};
		value(){mgc.push(this);}
		virtual ~value(){}
		virtual value* eval(env_tree*){return this;}
		virtual value* func(vector<value*>& args){return this;}
		virtual string toString(){return glb_symbol_map[this]+"@value";}

		virtual void gc_set_state(bool state){gc_state=state;}
		//bool static_val; useless
		bool auto_del;
		bool gc_state;

		static gc mgc;
		static value* nil;
		static value* cond;
		static value* quote;
		static value* lambda;
	private:
		value(const value&);
		value(value&&);
	};
	value::gc value::mgc;
	void symbol_map::bind(string name, value* val){
		map<string, value*>::iterator fd= map_sv.find(name);
		if(fd!=map_sv.end()) value::mgc.auto_collect(fd->second);
		map_sv[name]= val;
		map_vs[val]= name;
		value::mgc.uncollect(val);
	}


	//date types
	class number: public value{
	public:
		number(float n){num=n;}
		string toString(){
			stringstream n;
			n<<num;
			return n.str();
		}
		float num;
	};
	class str_type: public value{
	public:
		str_type(){}
		str_type(string _s){s=_s;}
		string toString(){return s+"@string";}
		string s;
	};


	class env_tree: public value{
	public:
		env_tree(value* _symbol, value* _data, env_tree* _parent=0){
			symbol= _symbol;
			data= _data;
			parent= _parent;
		}
		value* find(value* _symbol){
			if(_symbol==symbol)return data;
			if(parent==0)return value::nil;
			return parent->find(_symbol);
		}
		void gc_set_state(bool state){
			if(gc_state==state)return;
			gc_state= state;
			if(symbol!=0)symbol->gc_set_state(state);
			if(data!=0)data->gc_set_state(state);
			if(parent!=0)parent->gc_set_state(state);
		}
		value* symbol;
		value* data;
		env_tree* parent;
	};
	
	class symbol: public value{
	public:
		value* eval(env_tree* envt){return envt?envt->find(this):value::nil;}
		string toString(){return glb_symbol_map[this]+"@symbol";}
	};


	class list: public value{
	public:
		value* eval(env_tree* envt){
			//特殊表达式
			if(ldata.size()==0) return value::nil;
			if(ldata[0]==value::quote) return ldata[1];
			if(ldata[0]==value::lambda){
				//('lambda args exp env)
				if(ldata.size()==3) ldata.push_back(envt);
				return this;
			}
			if(ldata[0]==value::cond){
				//(cond ((bool) (exp)) ((bool) (exp)))
				if(ldata.size()<2)return value::nil;
				list* bloc= dynamic_cast<list*>(ldata[1]);
				if(bloc==0) return value::nil;
				for(size_t i=0; i<bloc->ldata.size(); ++i){
					list* each_enum= dynamic_cast<list*>(bloc->ldata[i]);
					if(each_enum==0) return value::nil;
					if(each_enum->ldata.size()<2) return value::nil;
					value* condition= each_enum->ldata[0];
					value* expression= each_enum->ldata[1];
					if(condition==0 || expression==0) return value::nil;
					if(condition->eval(envt)!=value::nil) return expression->eval(envt);
				}
				return value::nil;
			}
			//一般表达式
			//eval this as list
			list* applyed= new list;
			for(size_t i=0; i< ldata.size(); ++i){
				applyed->add_arg(ldata[i]->eval(envt));
			}
			value::mgc.auto_collect(applyed);

			if(applyed->ldata.size()==0){
				return value::nil;
			}else{
				//error here in can't find function
				value* ret= applyed->ldata[0]->func(applyed->ldata);
				return ret;
			}
		}
		value* func(vector<value*>& args){
			//eval this as lambda
			//('lambda args exp env)
			if(ldata[0]!=value::lambda){
				//this is not lambda
			}
			if(dynamic_cast<list*>(ldata[1])==0){
				//have no args
			}
			if(dynamic_cast<env_tree*>(ldata[3])==0){
				//have no env_tree
			}
			//make envt
			vector<value*>& fargs= dynamic_cast<list*>(ldata[1])->ldata;
			env_tree* begin= dynamic_cast<env_tree*>(ldata[3]);
			env_tree* back= begin;
			for(size_t i=0; i<fargs.size(); ++i){
				env_tree* temp= new env_tree(fargs[i],args[i+1],back); //args[0] is self
				value::mgc.auto_collect(temp);
				back= temp;
			}

			value* ret= ldata[2]->eval(back);

			return ret;
		}
		string toString(){
			string ret="(";
			for(size_t i=0; i< ldata.size(); ++i){
				ret+= ldata[i]->toString();
				if(i<ldata.size()-1) ret+= " ";
			}
			ret+= ")";
			return ret;
		}
		void gc_set_state(bool state){
			if(gc_state==state)return;
			gc_state= state;
			for(size_t i= 0; i<ldata.size(); ++i){
				ldata[i]->gc_set_state(state);
			}
		}
		list& add_arg(value* arg){
			ldata.push_back(arg);
			return *this;
		}
	public:
		vector<value*> ldata;//func bloc
		map<string, size_t> symbol_map;
	};

	class syntax{
	public:
		bool not_end(){return pos<end&&*pos!='\0';}
		char* inc_pos(){
			if(not_end())++pos;
			return pos;
		}
		bool peek_space(){
			if(*pos!=' '&&*pos!='\n')return false;
			while(not_end()&&(*pos==' '||*pos=='\n'))inc_pos();
			return true;
		}
		number* peek_number(){
			peek_space();
			double num= atof(pos);
			//skip
			if(*pos=='+'||*pos=='-') inc_pos();
			while(not_end()&&(*pos=='.' || (*pos>='0' && *pos<='9'))) inc_pos();
			if(*pos=='e'||*pos=='E') inc_pos();
			if(*pos=='+'||*pos=='-') inc_pos();
			while(not_end() && *pos>='0' && *pos<='9') inc_pos();

			number* ret= new number(float(num));
			value::mgc.auto_collect(ret);
			return ret;
		}
		str_type* peek_str(){
			peek_space();
			inc_pos();
			char* begin= pos;
			while(not_end()&&*pos!='"'){
				if(*pos=='\\') inc_pos();
				inc_pos();
			}
			string str(begin,pos);
			inc_pos();
			str_type* ret= new str_type(str);
			value::mgc.auto_collect(ret);
			return ret;
		}
		value* peek_symbol(){
			peek_space();
			char* begin=pos;
			while(not_end()&&*pos!=' '&&*pos!=')') inc_pos();
			string name(begin,pos);

			if(glb_symbol_map[name]==0){
				value* temp= new symbol();
				glb_symbol_map.bind(name,temp);
			}
			return glb_symbol_map[name];
		}
		value* peek_list(){
			peek_space();
			switch(*pos){
			case'+':case'-':case'.':
			case'0':case'1':case'2':case'3':case'4':
			case'5':case'6':case'7':case'8':case'9':
				return peek_number();
			case'"':
				return peek_str();
			case '(':
				{
					inc_pos();
					list* ret=new list;
					while(not_end() && *pos!=')' && pos<end){
						ret->add_arg(peek_list());
						peek_space();
					}
					value::mgc.auto_collect(ret);
					inc_pos();
					return ret;
				}
			default:
				return peek_symbol();
			}
		}
	public:
		char* pos;
		char* end;
	};
	
	value gnil;
	value* value::nil= &gnil;
	value gcond;
	value* value::cond= &gcond;
	value gquote;
	value* value::quote= &gquote;
	value glambda;
	value* value::lambda= &glambda;
/*	env_tree* base_env(){
		static env_tree* base=0;
		if(base!=0)return base;
		base= new env_tree(value::nil,value::nil,base);
		base= new env_tree(value::quote,value::quote,base);
		base= new env_tree(value::lambda,value::lambda,base);
		return base;
	}*/


	//-------------------------user file----------------------
#include<cmath>
	class eq_op: public value{
	public:
		value* func(vector<value*>& args){
			if(args.size()<3)return value::nil;
			if(args[1]==args[2]) return value::quote;//true
			return value::nil;
		}
		string toString(){return "eq";}
	};
	class num_eq_op: public value{
	public:
		value* func(vector<value*>& args){
			if(args.size()<3)return value::nil;
			number* arg1= dynamic_cast<number*>(args[1]);
			number* arg2= dynamic_cast<number*>(args[2]);
			if(arg1==0 || arg2==0) return value::nil;
			if(abs(arg1->num-arg2->num)<0.00001) return value::quote;//true
			return value::nil;
		}
		string toString(){return "add";}
	};
	class add_op: public value{
	public:
		value* func(vector<value*>& args){
			float sum=0;
			for(size_t i=1; i<args.size(); ++i){
				number* n= dynamic_cast<number*>(args[i]);
				if(n!=0) sum+=n->num;
			}
			value* ret= new number(sum);
			value::mgc.auto_collect(ret);
			return ret;
		}
		string toString(){return "add";}
	};
	class sub_op: public value{
	public:
		value* func(vector<value*>& args){
			number* base_on= dynamic_cast<number*>(args[1]);
			float sum= base_on? base_on->num: 0;
			for(size_t i=2; i<args.size(); ++i){
				number* n= dynamic_cast<number*>(args[i]);
				if(n!=0) sum-=n->num;
			}
			value* ret= new number(sum);
			value::mgc.auto_collect(ret);
			return ret;
		}
		string toString(){return "sub";}
	};
		class mul_op: public value{
	public:
		value* func(vector<value*>& args){
			float sum= 1;
			for(size_t i=1; i<args.size(); ++i){
				number* n= dynamic_cast<number*>(args[i]);
				if(n!=0) sum*=n->num;
			}
			value* ret= new number(sum);
			value::mgc.auto_collect(ret);
			return ret;
		}
		string toString(){return "mul";}
	};
	class div_op: public value{
	public:
		value* func(vector<value*>& args){
			number* base_on= dynamic_cast<number*>(args[1]);
			float sum= base_on? base_on->num: 0;
			for(size_t i=2; i<args.size(); ++i){
				number* n= dynamic_cast<number*>(args[i]);
				if(n!=0) sum/=n->num;
			}
			value* ret= new number(sum);
			value::mgc.auto_collect(ret);
			return ret;
		}
		string toString(){return "div";}
	};
	
	class pow_op: public value{
	public:
		value* func(vector<value*>& args){
			number* arg1= dynamic_cast<number*>(args[1]);
			number* arg2= dynamic_cast<number*>(args[2]);

			value* ret= new number(float(pow(arg1->num, arg2->num)));
			value::mgc.auto_collect(ret);
			return ret;
		}
		string toString(){return "pow";}
	};
	
	class sqrt_op: public value{
	public:
		value* func(vector<value*>& args){
			number* arg1= dynamic_cast<number*>(args[1]);

			value* ret= new number(float(sqrt(arg1->num)));
			value::mgc.auto_collect(ret);
			return ret;
		}
		string toString(){return "sqrt";}
	};

	class sin_op: public value{
	public:
		value* func(vector<value*>& args){
			number* arg1= dynamic_cast<number*>(args[1]);

			value* ret= new number(float(sin(arg1->num)));
			value::mgc.auto_collect(ret);
			return ret;
		}
		string toString(){return "sin";}
	};

	class cos_op: public value{
	public:
		value* func(vector<value*>& args){
			number* arg1= dynamic_cast<number*>(args[1]);

			value* ret= new number(float(cos(arg1->num)));
			value::mgc.auto_collect(ret);
			return ret;
		}
		string toString(){return "cos";}
	};
	class log_op: public value{
	public:
		value* func(vector<value*>& args){
			number* arg1= dynamic_cast<number*>(args[1]);

			value* ret= new number(float(log(arg1->num)));
			value::mgc.auto_collect(ret);
			return ret;
		}
		string toString(){return "log";}
	};
	class abs_op: public value{
	public:
		value* func(vector<value*>& args){
			number* arg1= dynamic_cast<number*>(args[1]);

			value* ret= new number(float(abs(arg1->num)));
			value::mgc.auto_collect(ret);
			return ret;
		}
		string toString(){return "abs";}
	};
	class system_func: public value{
	public:
		value* func(vector<value*>& args){
			str_type* s= dynamic_cast<str_type*>(args[1]);
			number* ret;
			if(s!=0){
				ret= new number(float(system(s->s.c_str())));
			}else{
				ret= new number(float(system(args[1]->toString().c_str())));
			}
			value::mgc.auto_collect(ret);
			return ret;
		}
	};

	class set_glb_val_func: public value{
	public:
		value* func(vector<value*>& args){
			if(args.size()<3) return value::nil;
			str_type* s= dynamic_cast<str_type*>(args[1]);
			if(s==0) return value::nil;
			glb_symbol_map.bind(s->s, args[2]);
			value::mgc.uncollect(args[2]);
			return args[2];
		}
	};

	void init(){
		glb_symbol_map.bind("t",new value);
		glb_symbol_map.bind("nil",value::nil);
		glb_symbol_map.bind("cond",value::cond);
		glb_symbol_map.bind("quote",value::quote);
		glb_symbol_map.bind("lambda",value::lambda);
		
		glb_symbol_map.bind("add",new add_op);
		glb_symbol_map.bind("sub",new sub_op);
		glb_symbol_map.bind("mul",new mul_op);
		glb_symbol_map.bind("div",new div_op);
		glb_symbol_map.bind("pow",new pow_op);
		glb_symbol_map.bind("sqrt",new sqrt_op);
		glb_symbol_map.bind("sin",new sin_op);
		glb_symbol_map.bind("cos",new cos_op);
		glb_symbol_map.bind("log",new log_op);
		glb_symbol_map.bind("abs",new abs_op);
		
		glb_symbol_map.bind("pi", new number(float(3.14159265358979323846)));
		glb_symbol_map.bind("e", new number(float(2.71828182845904523536)));

		glb_symbol_map.bind("system",new system_func);
		glb_symbol_map.bind("setglb",new set_glb_val_func);

	}

}


void main(){
	using namespace pl;
	init();
	char strbuff[1024];
	//bool on_debug= false;
	//if(on_debug){
	//	//(+ 40 2)
	//	number na(40);
	//	number nb(2);
	//	list add_40_2;
	//	add_40_2.add_arg(glb_symbol_map["add"]).add_arg(&na).add_arg(&nb);

	//	cout<<add_40_2.toString()<<endl;
	//	cout<<add_40_2.eval(base_env())->toString()<<endl<<endl;

	//	//((lambda (a) (+ a 2)) 40)
	//	symbol sa,sb;
	//	list runinc,inc,inc1,inc2;
	//	inc1.add_arg(&sa);
	//	inc2.add_arg(glb_symbol_map["add"]).add_arg(&sa).add_arg(&nb);
	//	inc.add_arg(glb_symbol_map["lambda"]).add_arg(&inc1).add_arg(&inc2);
	//	runinc.add_arg(&inc).add_arg(&na);
	//	cout<<runinc.toString()<<endl;
	//	cout<<runinc.eval(base_env())->toString()<<endl<<endl;


	//	//(((lambda (a) (lambda (b) (+ a b))) 40) 2)
	//	//^l123     ^l4 ^l5     ^l6 ^l7

	//	list l1,l2,l3,l4,l5,l6,l7;

	//	l1.add_arg(&l2.add_arg(
	//		&l3.add_arg(glb_symbol_map["lambda"]).add_arg(&l4.add_arg(&sa)).add_arg(
	//		&l5.add_arg(glb_symbol_map["lambda"]).add_arg(&l6.add_arg(&sb)).add_arg(
	//		&l7.add_arg(glb_symbol_map["add"]).add_arg(&sa).add_arg(&sb)
	//		)
	//		)
	//		).add_arg(&na)).add_arg(&nb);

	//	cout<<l1.toString()<<endl;
	//	cout<<l1.eval(base_env())->toString()<<endl<<endl;


	//	//syntax
	//	strcpy_s(strbuff, "(((lambda (a) (lambda (b) (add a b))) 20) 22)");
	//	syntax s;
	//	s.pos=strbuff;
	//	s.end=s.pos+1024;
	//	value* syntax1= s.peek_list();

	//	cout<<syntax1->toString()<<endl;
	//	cout<<syntax1->eval(base_env())->toString()<<endl<<endl;
	//}

	env_tree* base_env= new env_tree(value::nil,value::nil,0);
	while(1){
		cout<<"input lisp:";
		gets_s(strbuff);
		if(strcmp(strbuff,"exit")==0) break;

		syntax s2;
		s2.pos=strbuff;
		s2.end=s2.pos+1024;
		value* syntax2= s2.peek_list();
		value::mgc.uncollect(syntax2);

		cout<<syntax2->toString()<<endl;
		cout<<syntax2->eval(base_env)->toString()<<endl<<endl;
		cout<<"gc pool size:"<<value::mgc.pool.size()<<endl<<endl;

		value::mgc.auto_collect(syntax2);
	}

	system("pause");
}