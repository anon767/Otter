// -*- c++ -*-
/********************************************************************
 * AUTHORS: Vijay Ganesh
 *
 * BEGIN DATE: November, 2005
 *
 * LICENSE: Please view LICENSE file in the home dir of this Program
 ********************************************************************/

// to get the PRIu64 macro from inttypes, this needs to be defined.
#ifndef __STDC_FORMAT_MACROS
  #define __STDC_FORMAT_MACROS
#endif
#include <inttypes.h>
#include <cmath>
#include "../STPManager/STPManager.h"
#include "../printer/SMTLIBPrinter.h"

namespace BEEV
{
  class ASTInteriorCleanUp
  {
  public:
    void operator()(ASTInternal * p) {
      std::tr1::shared_ptr<STPMgr> bm = p->GetSTPMgr();
      if (bm) {
        bm->_interior_unique_table.erase(static_cast<ASTInterior *>(p));
      }
      delete p;
    }
  };

  ASTNode STPMgr::LookupOrCreateInterior(ASTInterior *n_ptr)
  {
    ASTInteriorSet::iterator it = _interior_unique_table.find(n_ptr);
    if (it == _interior_unique_table.end())
      {
        // Make a new ASTInterior node We want (NOT alpha) always to
        // have alpha.nodenum + 1.
        if (n_ptr->GetKind() == NOT)
          {
        	// The internal node can't be a NOT, because then we'd add
        	// 1 to the NOT's node number, meaning we'd hit an even number,
        	// which could duplicate the next newNodeNum().
        	assert(n_ptr->GetChildren()[0].GetKind() != NOT);
       		n_ptr->SetNodeNum(n_ptr->GetChildren()[0].GetNodeNum() + 1);
          }
        else
          {
            n_ptr->SetNodeNum(NewNodeNum());
          }
        ASTNode n(n_ptr, ASTInteriorCleanUp());
        _interior_unique_table[n_ptr] = ASTWeakNode(n);
        return n;
      }
    else
      {
        // Delete the temporary node, and return the found node.
        delete n_ptr;
        ASTNode n(it->second);
        return n;
      }
  }

  ASTNode STPMgr::CreateInteriorNode(Kind kind,
                                          const ASTVec & back_children)
  {
    // create a new node.  Children will be modified.
    ASTInterior *n_ptr = new ASTInterior(kind, _self);

    // insert back_children at end of front_children
    ASTVec &front_children = n_ptr->_children;
	front_children.reserve(front_children.size()+ back_children.size());

    front_children.insert(front_children.end(), 
                          back_children.begin(), 
                          back_children.end());

    // check for undefined nodes.
    ASTVec::const_iterator it_end = front_children.end();
    for (ASTVec::const_iterator it = front_children.begin();
	 it != it_end; it++)
      {
        if (it->IsNull())
          {
            FatalError("CreateInteriorNode:"\
                       "Undefined childnode in CreateInteriorNode: ", 
                       ASTUndefined);
          }
      }

    return LookupOrCreateInterior(n_ptr);
  }

  ostream &operator<<(ostream &os, const ASTNodeMap &nmap)
  {
    ASTNodeMap::const_iterator iend = nmap.end();
    for (ASTNodeMap::const_iterator i = nmap.begin(); i != iend; i++)
      {
        os << "Key: " << i->first << endl;
        os << "Value: " << i->second << endl;
      }
    return os;
  }

  ////////////////////////////////////////////////////////////////
  // STPMgr member functions to create ASTSymbol and ASTBVConst
  ////////////////////////////////////////////////////////////////
  ASTNode STPMgr::LookupOrCreateSymbol(const char * const name)
  {
    ASTSymbol temp_sym(name, _self);
    return LookupOrCreateSymbol(temp_sym);
  }

  class ASTSymbolCleanUp
  {
  private:
    const char * _name;
  public:
    ASTSymbolCleanUp(const char * name) : _name(name) {}
    void operator()(ASTInternal * p) {
      std::tr1::shared_ptr<STPMgr> bm = p->GetSTPMgr();
      if (bm) {
        bm->_symbol_unique_table.erase(static_cast<ASTSymbol *>(p));
      }
      free(const_cast<char *>(_name));
      delete p;
    }
  };

  ASTNode STPMgr::LookupOrCreateSymbol(ASTSymbol& s)
  {
    ASTSymbol *s_ptr = &s; // it's a temporary key.

    ASTSymbolSet::const_iterator it = _symbol_unique_table.find(s_ptr);
    if (it == _symbol_unique_table.end())
      {
        // We don't know if s_ptr owns it's name string,
        // so make a new ASTSymbol with duplicated string,
        // and use a deleter to delete it along with ASTSymbol.
        const char * name = strdup(s_ptr->GetName());
        ASTSymbol * s_ptr1 = new ASTSymbol(name, _self);
        s_ptr1->SetNodeNum(NewNodeNum());
        s_ptr1->_value_width = s_ptr->_value_width;
        ASTNode n(s_ptr1, ASTSymbolCleanUp(name));
        _symbol_unique_table[s_ptr1]= ASTWeakNode(n);
        return n;
      }
    else
      {
        // return symbol found in table.
        ASTNode n(it->second);
        return n;
      }
  } // End of LookupOrCreateSymbol

  bool STPMgr::LookupSymbol(ASTSymbol& s)
  {
    ASTSymbol* s_ptr = &s; // it's a temporary key.

    if (_symbol_unique_table.find(s_ptr) == 
        _symbol_unique_table.end())
      return false;
    else
      return true;
  }

  bool STPMgr::LookupSymbol(const char * const name)
  {
    ASTSymbol s(name, _self);
    ASTSymbol* s_ptr = &s; // it's a temporary key.

    if (_symbol_unique_table.find(s_ptr) ==
        _symbol_unique_table.end())
      return false;
    else
      return true;
  }

  bool STPMgr::LookupSymbol(const char * const name, ASTNode& output)
  {
    ASTSymbol temp_sym(name, _self);
    ASTSymbolSet::const_iterator it = _symbol_unique_table.find(&temp_sym);
    if (it != _symbol_unique_table.end())
      {
        output = it->second;
        return true;
      }
  return false;
  }




  //Create a ASTBVConst node
  ASTNode STPMgr::CreateBVConst(unsigned int width, 
				unsigned long long int bvconst)
  {
    if (width > (sizeof(unsigned long long int) << 3) || width <= 0)
      FatalError("CreateBVConst: "\
                 "trying to create bvconst using "\
		 "unsigned long long of width: ", 
                 ASTUndefined, width);


    // We create a single bvconst that gets reused.
    if (NULL == CreateBVConstVal)
      CreateBVConstVal = CONSTANTBV::BitVector_Create(65, true);
    CreateBVConstVal  = CONSTANTBV::BitVector_Resize(CreateBVConstVal,width);
    CONSTANTBV::BitVector_Empty(CreateBVConstVal);

    unsigned long c_val = (~((unsigned long) 0)) & bvconst;
    unsigned int copied = 0;

    // sizeof(unsigned long) returns the number of bytes in unsigned
    // long. In order to convert it to bits, we need to shift left by
    // 3. Hence, sizeof(unsigned long) << 3

    //The algo below works as follows: It starts by copying the
    //lower-order bits of the input "bvconst" in chunks of size =
    //number of bits in unsigned long. The variable "copied" keeps
    //track of the number of chunks copied so far

    const int shift_amount = (sizeof(unsigned long) << 3);
    while (copied + shift_amount < width)
      {
        CONSTANTBV::BitVector_Chunk_Store(CreateBVConstVal, shift_amount, copied, c_val);
        bvconst = bvconst >> shift_amount;
        c_val = (~((unsigned long) 0)) & bvconst;
        copied += shift_amount;
      }
    CONSTANTBV::BitVector_Chunk_Store(CreateBVConstVal, width - copied, copied, c_val);

    ASTBVConst temp_bvconst(CreateBVConstVal, width,ASTBVConst::CBV_MANAGED_OUTSIDE, _self);
    return  ASTNode(LookupOrCreateBVConst(temp_bvconst));

  }

  ASTNode STPMgr::charToASTNode(unsigned char* strval, int base , int bit_width)
  {
    assert ((2 == base || 10 == base || 16 == base));
    assert (bit_width > 0);

    // We create a single bvconst that gets reused.
    if (NULL == CreateBVConstVal)
      CreateBVConstVal = CONSTANTBV::BitVector_Create(65, true);
    CreateBVConstVal  = CONSTANTBV::BitVector_Resize(CreateBVConstVal,bit_width);
    CONSTANTBV::BitVector_Empty(CreateBVConstVal);

    CONSTANTBV::ErrCode e;
    if (2 == base)
      {
        e = CONSTANTBV::BitVector_from_Bin(CreateBVConstVal,
                                           strval);
      }
    else if (10 == base)
      {
        e = CONSTANTBV::BitVector_from_Dec(CreateBVConstVal,
                                           strval);
      }
    else if (16 == base)
      {
        e = CONSTANTBV::BitVector_from_Hex(CreateBVConstVal,
                                           strval);
      }
    else
      {
        e = CONSTANTBV::ErrCode_Pars;
      }

    if (0 != e)
      {
        cerr << "CreateBVConst: " << BitVector_Error(e);
        FatalError("", ASTUndefined);
      }

    ASTBVConst temp_bvconst(CreateBVConstVal, bit_width, ASTBVConst::CBV_MANAGED_OUTSIDE, _self);
    ASTNode n = LookupOrCreateBVConst(temp_bvconst);
    return n;
  }

  ASTNode STPMgr::CreateBVConst(string strval, int base, int bit_width)
  {
    assert (bit_width > 0);

    return charToASTNode((unsigned char*)strval.c_str(), base , bit_width);
  }

  //Create a ASTBVConst node from a char*
  ASTNode STPMgr::CreateBVConst(const char* const strval, int base)
  {
    assert ((2 == base || 10 == base || 16 == base));

    size_t width = strlen((const char *) strval);

    //FIXME Tim: Earlier versions of the code assume that the length of
    //binary strings is 32 bits.
    if (10 == base)
      width = 32;
    if (16 == base)
      width = width * 4;

    return charToASTNode((unsigned char*)strval, base , width);
  }

  //NB Assumes that it will destroy the bitvector passed to it
  ASTNode STPMgr::CreateBVConst(CBV bv, unsigned width)
  {
    ASTBVConst temp_bvconst(bv, width, ASTBVConst::CBV_MANAGED_OUTSIDE, _self);
    ASTNode n(LookupOrCreateBVConst(temp_bvconst));
    CONSTANTBV::BitVector_Destroy(bv);
    return n;
  }

  ASTNode STPMgr::CreateZeroConst(unsigned width)
  {
	  assert(width > 0);
	  if (zeroes.size() == 0)
	  {
		  zeroes.push_back(ASTNode()); // null
		  for (int i =1; i < 65;i++)
			  zeroes.push_back(CreateZeroConst(i));
	  }

	if (width < zeroes.size())
		return zeroes[width];
	else
	{
		CBV z = CONSTANTBV::BitVector_Create(width, true);
		return CreateBVConst(z, width);
	}
  }


  ASTNode STPMgr::CreateOneConst(unsigned width)
  {
	  assert(width > 0);
	  if (ones.size() == 0)
	  {
		  ones.push_back(ASTNode()); // null
		  for (int i =1; i < 65;i++)
			  ones.push_back(CreateOneConst(i));
	  }

	if (width < ones.size())
		return ones[width];
	else
	{
	    CBV o = CONSTANTBV::BitVector_Create(width, true);
	    CONSTANTBV::BitVector_increment(o);

	    return CreateBVConst(o, width);
	}
  }

  ASTNode STPMgr::CreateTwoConst(unsigned width)
  {
    CBV two = CONSTANTBV::BitVector_Create(width, true);
    CONSTANTBV::BitVector_increment(two);
    CONSTANTBV::BitVector_increment(two);

    return CreateBVConst(two, width);
  }

  ASTNode STPMgr::CreateMaxConst(unsigned width)
  {
	  assert(width > 0);
	  if (max.size() == 0)
	  {
		  max.push_back(ASTNode()); // null
		  for (int i =1; i < 65;i++)
			  max.push_back(CreateMaxConst(i));
	  }

	if (width < max.size())
		return max[width];
	else
	{
		  CBV max = CONSTANTBV::BitVector_Create(width, false);
	    CONSTANTBV::BitVector_Fill(max);

	    return CreateBVConst(max, width);
	}
  }

  class ASTBVConstCleanUp
  {
  public:
    void operator()(ASTInternal * p) {
      std::tr1::shared_ptr<STPMgr> bm = p->GetSTPMgr();
      if (bm) {
        bm->_bvconst_unique_table.erase(static_cast<ASTBVConst *>(p));
      }
      delete p;
    }
  };

  //To ensure unique BVConst nodes, lookup the node in unique-table
  //before creating a new one.
  ASTNode STPMgr::LookupOrCreateBVConst(ASTBVConst &s)
  {
    // Do an explicit lookup to see if we need to create a copy of the string.
    ASTBVConstSet::const_iterator it;
    if ((it = _bvconst_unique_table.find(&s)) == _bvconst_unique_table.end())
      {
        // Make a new ASTBVConst with duplicated constant.

        ASTBVConst * s_copy = new ASTBVConst(s);
        s_copy->SetNodeNum(NewNodeNum());

        ASTNode n(s_copy, ASTBVConstCleanUp());
        _bvconst_unique_table[s_copy] = ASTWeakNode(n);
        return n;
      }
    else
      {
        // return constant found in table.
        ASTNode n(it->second);
        return n;
      }
  }

  ////////////////////////////////////////////////////////////////
  //
  //  IO manipulators for Lisp format printing of AST.
  //
  ////////////////////////////////////////////////////////////////

  // FIXME: Additional controls
  //   * Print node numbers  (addresses/nums)
  //   * Printlength limit
  //   * Printdepth limit

  /** Print a vector of ASTNodes in lisp format */
  ostream &LispPrintVec(ostream &os, const ASTVec &v, int indentation)
  {
    // Print the children
    ASTVec::const_iterator iend = v.end();
    for (ASTVec::const_iterator i = v.begin(); i != iend; i++)
      {
        i->LispPrint_indent(os, indentation);
      }
    return os;
  }

  ostream &LispPrintVecSpecial(ostream &os, 
			       const vector<const ASTNode*> &v, 
			       int indentation)
  {
    // Print the children
    vector<const ASTNode*>::const_iterator iend = v.end();
    for (vector<const ASTNode*>::const_iterator i = v.begin(); i != iend; i++)
      {
        (*i)->LispPrint_indent(os, indentation);
      }
    return os;
  }

  //add an assertion to the current logical context
  void STPMgr::AddAssert(const ASTNode& assert, int assertion_bucket_key)
  {
    if (!(is_Form_kind(assert.GetKind()) 
	  && BOOLEAN_TYPE == assert.GetType()))
      {
        FatalError("AddAssert:Trying to assert a non-formula:", assert);
      }

    IntToASTVecMap * AssertsMap;
    //if the stack of assertion contexts is not empty, then take the
    //top ASTVec and add the input assert to it
    if(!_asserts.empty())
      {
	AssertsMap = _asserts.back();
      }
    else
      {
	AssertsMap = new IntToASTVecMap();
	_asserts.push_back(AssertsMap);
      }

    ASTVec * v;
    if(AssertsMap->find(assertion_bucket_key) != AssertsMap->end())
      {
	v = (*AssertsMap)[assertion_bucket_key];
	v->push_back(assert);
      }
    else
      {
	v = new ASTVec();
	v->push_back(assert);
	(*AssertsMap)[assertion_bucket_key] = v;
      }

    //     //if the stack of ASTVec is not empty, then take the top ASTVec
    //     //and add the input assert to it
    //     if (!_asserts.empty())
    //       {
    //         v = _asserts.back();
    //         //v->push_back(TransformFormula(assert));
    //         v->push_back(assert);
    //       }
    //     else
    //       {
    //         //else create a logical context, and add it to the top of the
    //         //stack
    //         v = new ASTVec();
    //         //v->push_back(TransformFormula(assert));
    //         v->push_back(assert);
    //         _asserts.push_back(v);
    //       }
  }

  void STPMgr::Push(void)
  {
    //     ASTVec * v;
    //     v = new ASTVec();
    IntToASTVecMap * v;
    v = new IntToASTVecMap();
    _asserts.push_back(v);
  }

  void STPMgr::Pop(void)
  {
    if (!_asserts.empty())
      {
	IntToASTVecMap * c = _asserts.back();
	IntToASTVecMap::iterator it = c->begin(), itend = c->end();
	for(;it!=itend;it++)
	  {
	    ASTVec * cc = (*it).second;
	    cc->clear();
	    delete cc;
	  }
	delete c;
	_asserts.pop_back();
	
        // ASTVec * c = _asserts.back();
	//         //by calling the clear function we ensure that the
	//         ref count is //decremented for the ASTNodes stored
	//         in c
	//         c->clear();
	//         delete c;
	//         _asserts.pop_back();
      }
  }

  void STPMgr::AddQuery(const ASTNode& q)
  {
    //_current_query = TransformFormula(q);
    //cerr << "\nThe current query is: " << q << endl;
    _current_query = q;
  }

  const ASTNode STPMgr::PopQuery()
  {
    ASTNode q = _current_query;
    _current_query = ASTTrue;
    return q;
  }

  const ASTNode STPMgr::GetQuery()
  {
    return _current_query;
  }

  const ASTVec STPMgr::GetAsserts(void)
  {
    vector<IntToASTVecMap *>::iterator it = _asserts.begin();
    vector<IntToASTVecMap *>::iterator itend = _asserts.end();

    ASTVec v;
    for(; it != itend; it++)
      {
	IntToASTVecMap::iterator jt = (*it)->begin();
	IntToASTVecMap::iterator jtend = (*it)->end();
	
	for(; jt != jtend; jt++)
	  {
	    ASTVec * cc = (*jt).second; 
	    if(NULL != cc && !cc->empty())
	      {
		v.insert(v.end(), cc->begin(), cc->end());
	      }
	  }
      }

    return v;
    // vector<ASTVec *>::iterator it = _asserts.begin();
    //     vector<ASTVec *>::iterator itend = _asserts.end();
    
    //     ASTVec v;
    //     for (; it != itend; it++)
    //       {
    //         if (!(*it)->empty())
    //           v.insert(v.end(), (*it)->begin(), (*it)->end());
    //       }
    //     return v;
  }

  const ASTVec STPMgr::GetAsserts_WithKey(int key)
  {
    vector<IntToASTVecMap *>::iterator it = _asserts.begin();
    vector<IntToASTVecMap *>::iterator itend = _asserts.end();

    ASTVec v;
    for(; it != itend; it++)
      {
	IntToASTVecMap * assert_map = (*it);
	ASTVec * cc = (*assert_map)[key];
	if(!cc->empty())
	  {
	    v.insert(v.end(), cc->begin(), cc->end());
	  }
      }

    return v;   
  }

  // //Create a new variable of ValueWidth 'n'
  //   ASTNode STPMgr::NewArrayVar(unsigned int index, unsigned int value)
  //   {
  //     std::string c("v");
  //     char d[32];
  //     sprintf(d, "%d", _symbol_count++);
  //     std::string ccc(d);
  //     c += "_writearray_" + ccc;

  //     ASTNode CurrentSymbol = CreateSymbol(c.c_str());
  //     CurrentSymbol.SetValueWidth(value);
  //     CurrentSymbol.SetIndexWidth(index);
  //     return CurrentSymbol;
  //   } //end of NewArrayVar()

  //prints statistics for the ASTNode
  void STPMgr::ASTNodeStats(const char * c, const ASTNode& a)
  {
    if (!UserFlags.stats_flag)
      return;

    StatInfoSet.clear();
    //print node size:

    cout << "[" << GetRunTimes()->getDifference() << "]" <<  "Printing: " << c;
    if (UserFlags.print_nodes_flag)
      {
        //a.PL_Print(cout,0);
        //cout << endl;
        cout << a << endl;
      }
    cout << "Node size is: ";
    cout << NodeSize(a) << endl;
  }

  unsigned int STPMgr::NodeSize(const ASTNode& a, bool clearStatInfo)
  {
    if (clearStatInfo)
      StatInfoSet.clear();

    ASTNodeSet::iterator it;
    if ((it = StatInfoSet.find(a)) != StatInfoSet.end())
      //has already been counted
      return 0;

    //record that you have seen this node already
    StatInfoSet.insert(a);
    // cout << "Number of bytes per Node is: ";
    // cout << sizeof(*(a._int_node_ptr)) << endl;

    //leaf node has a size of 1
    if (a.Degree() == 0)
      return 1;

    unsigned newn = 1;
    const ASTVec& c = a.GetChildren();
    for (ASTVec::const_iterator it = c.begin(), itend = c.end(); it != itend; it++)
      newn += NodeSize(*it);
    return newn;
  }

  bool STPMgr::VarSeenInTerm(const ASTNode& var, const ASTNode& term)
  {
    if (READ == term.GetKind() 
        && WRITE == term[0].GetKind() 
        && !GetRemoveWritesFlag())
      {
        return false;
      }

    if (READ == term.GetKind() 
        && WRITE == term[0].GetKind() 
        && GetRemoveWritesFlag())
      {
        return true;
      }

    ASTNodeMap::iterator it;
    if ((it = TermsAlreadySeenMap.find(term)) != TermsAlreadySeenMap.end())
      {
        if (it->second == var)
          {
            return false;
          }
      }

    if (var == term)
      {
        return true;
      }

    for (ASTVec::const_iterator it = term.begin(),
	   itend = term.end(); it != itend; it++)
      {
        if (VarSeenInTerm(var, *it))
          {
            return true;
          }
        else
          {
            TermsAlreadySeenMap[*it] = var;
          }
      }

    TermsAlreadySeenMap[term] = var;
    return false;
  }//End of VarSeenInTerm

  
  ASTNode STPMgr::NewParameterized_BooleanVar(const ASTNode& var,
                                              const ASTNode& constant)
  {
    ostringstream outVar;
    ostringstream outNum;
    //Get the name of Boolean Var
    var.PL_Print(outVar);
    constant.PL_Print(outNum);
    std::string str(outVar.str());
    str += "(";
    str += outNum.str();
    str += ")";
    ASTNode CurrentSymbol = CreateSymbol(str.c_str(),0,0);
    return CurrentSymbol;
  } // End of NewParameterized_BooleanVar()

  
  //If ASTNode remain with references (somewhere), this will segfault.
  STPMgr::~STPMgr() {
 		ClearAllTables();

 		  printer::NodeLetVarMap.clear();
 		  printer::NodeLetVarVec.clear();
 		  printer::NodeLetVarMap1.clear();

 		delete runTimes;
 		runTimes = NULL;

 		if (NULL != CreateBVConstVal)
 			CONSTANTBV::BitVector_Destroy(CreateBVConstVal);

 		vector<IntToASTVecMap*>::iterator it = _asserts.begin();
 		vector<IntToASTVecMap*>::iterator itend = _asserts.end();

 		for (; it != itend; it++) {
 			IntToASTVecMap * j = (*it);
 			for (IntToASTVecMap::iterator it2 = j->begin(); it2 != j->end(); it2++) {
 				delete (it2->second);
 			}
 			delete j;
 		}

 		delete hashingNodeFactory;
 	}
}; // end namespace beev

