#if 0

#include<vector>
using namespace std;
namespace pl{
//垃圾回收 标记-删除(暂时不写)
//链接 替换
//引用计数 延迟复制(暂时不写)


//基础类-allocator
	template<typename T>
	class allocator_base{
	public:
		typedef T value_type;
		typedef value_type* pointer;
		typedef value_type const* const_pointer;
		typedef value_type& reference;
		typedef value_type const& const_reference;
		static pointer create(){return new T();}
		static pointer create(const value_type& x){return new T(x);}
		static void destroy(pointer x){delete x;}
	};
	template<typename T, template<class>class allocator>
	class auto_life{
	public:
		auto_life(){ptr=0;}
		auto_life(typename allocator<T>::pointer p){ptr=p;}
		~auto_life(){allocator<T>::destroy(ptr);}
		T* operator->()const{return &*ptr;}
		T& operator*()const{return *ptr;}
		T& operator T()const{return *ptr;}
		typename allocator<T>::pointer ptr;
	private:
		void operator=(const auto_life<T,allocator>&);
	};
	namespace lispl{
		template<typename T>
		class allocator: public allocator_base<T>{};
		class value{public: ~value(){} virtual bool eq(const value&)=0;};

//基础类-lispl_node
	class lispl_node{
	public:
		//多线程互斥锁支持
		//垃圾回收(标记-删除)支持
		//引用计数 延迟复制支持(计数器 读写状态)
		allocator<lispl_node>::pointer copy(){return real_copy();}
		allocator<lispl_node>::pointer real_copy(){
			return allocator<lispl_node>::create(*this);
		}
		//基础操作支持car cdr atom eq eval 
		allocator<lispl_node>::pointer car(){
			return subs.back();
		}
		allocator<lispl_node>::pointer cdr(){
			allocator<lispl_node>::pointer ret;
			ret= copy();
			ret->subs.pop_back();
			return ret;
		}
		bool atom()const{
			return isatom;
		}
		bool eq(const lispl_node& q)const{
			if(q.atom()!=atom())return false;
			if(atom()) return val->eq(val);
			unsigned int min= q.subs.size()<subs.size()? q.subs.size(): subs.size();
			for(unsigned int i=0; i!=min;++i){
				if( !q.subs[i]->eq(*subs[i]) ) return false;
			}
			return true;
		}

		bool isatom;
		//value if atom
		auto_life<value,allocator> val;//生存周期与node相同
		//if not atom 子节点 注意 存储顺序和实际顺序是相反的 back 是第一个元素
		vector<allocator<lispl_node>::pointer> subs;
	};

	}

}

#endif