// -*- c++ -*-
/********************************************************************
 * AUTHORS: Vijay Ganesh
 *
 * BEGIN DATE: November, 2005
 *
 * LICENSE: Please view LICENSE file in the home dir of this Program
 ********************************************************************/
#ifndef ASTINTERNAL_H
#define ASTINTERNAL_H

#include <tr1/memory>

/********************************************************************
 *  This file gives the class description of the ASTInternal class  *
 ********************************************************************/
namespace BEEV
{
  class STPMgr;

  /******************************************************************
   * struct enumeration:                                            *
   *                                                                *
   * Templated class that allows you to define the number of bytes  *
   * (using class T below) for the enumerated type class E.         *
   ******************************************************************/
  template <class E, class T>
  struct enumeration
  {
    typedef T type;
    typedef E enum_type;

    enumeration() : e_(E())
    {}
    
    enumeration(E e) : e_(static_cast<T>(e))
    {}
    
    operator E() const
    { return static_cast<E>(e_); }
    
  private:
    T e_;
  }; //end of Enumeration struct

  /******************************************************************
   * Class ASTInternal:                                             *
   *                                                                *
   * Abstract base class for internal node representation. Requires *
   * Kind and ChildNodes so same traversal works on all nodes.      *
   ******************************************************************/
  class ASTInternal
  {
    friend class ASTNode;
    friend class CNFMgr;

  protected:
    /****************************************************************
     * Protected Data                                               *
     ****************************************************************/
   
    // Kind. It's a type tag and the operator.
    enumeration<Kind,unsigned char> _kind;

    // A weak reference to the owner STPMgr
    std::tr1::weak_ptr<STPMgr> _bm;

    //Nodenum is a unique positive integer for the node.  The nodenum
    //of a node should always be greater than its descendents (which
    //is easily achieved by incrementing the number each time a new
    //node is created).
    unsigned int _node_num;

    /*******************************************************************
     * ASTNode is of type BV      <==> ((indexwidth=0)&&(valuewidth>0))*
     * ASTNode is of type ARRAY   <==> ((indexwidth>0)&&(valuewidth>0))*
     * ASTNode is of type BOOLEAN <==> ((indexwidth=0)&&(valuewidth=0))*
     *                                                                 *
     * Width of the index of an array. Positive for array, 0 otherwise *
     *******************************************************************/
    unsigned int  _index_width;


    /*******************************************************************
     * ASTNode is of type BV      <==> ((indexwidth=0)&&(valuewidth>0))*
     * ASTNode is of type ARRAY   <==> ((indexwidth>0)&&(valuewidth>0))*
     * ASTNode is of type BOOLEAN <==> ((indexwidth=0)&&(valuewidth=0))*
     *                                                                 *
     * Number of bits of bitvector. +ve for array/bitvector,0 otherwise*
     *******************************************************************/
    unsigned int  _value_width;

    /****************************************************************
     * Protected Member Functions                                   *
     ****************************************************************/

    // Copying assign operator.  Also copies contents of children.
    ASTInternal& operator=(const ASTInternal &int_node);

    // Abstract virtual print function for internal node. c_friendly
    // is for printing hex. numbers that C compilers will accept
    virtual void nodeprint(ostream& os, bool c_friendly = false)
    {
      os << "*";
    }
    ;

    // Treat the result as const pleases
    virtual Kind GetKind() const
    {
      return _kind;
    }

    // Get the child nodes of this node
    virtual ASTVec const &GetChildren() const = 0;

    // Constructor
    ASTInternal(std::tr1::weak_ptr<STPMgr> bm, int nodenum = 0) :
      _kind(UNDEFINED),
      _bm(bm),
      _node_num(nodenum), 
      _index_width(0), _value_width(0)
    {
    }

    // Constructor (kind only, empty children, int nodenum)
    ASTInternal(Kind kind, std::tr1::weak_ptr<STPMgr> bm, int nodenum = 0) :
      _kind(kind),
      _bm(bm),
      _node_num(nodenum),
      _index_width(0), _value_width(0)
    {
    }

    // Copy constructor.  This copies the contents of the child nodes
    // array, along with everything else.  Assigning the smart pointer,
    // ASTNode, does NOT invoke this; This should only be used for
    // temporary hash keys before uniquefication.
    // FIXME:  I don't think children need to be copied.
    ASTInternal(const ASTInternal &int_node) :
      _kind(int_node._kind),
      //_children(int_node._children),
      _bm(int_node._bm),
      _node_num(int_node._node_num), 
      _index_width(int_node._index_width),
      _value_width(int_node._value_width)
    {
    }

  public:

    /****************************************************************
     * Public Member Functions                                      *
     ****************************************************************/

    // Destructor (does nothing, but is declared virtual here).
    virtual ~ASTInternal()
    {
    }

    std::tr1::shared_ptr<STPMgr> GetSTPMgr() const
    {
      return _bm.lock();
    }

    int GetNodeNum() const
    {
      return _node_num;
    } //End of GetNodeNum()


    virtual bool isSimplified() const
    {
      return false;
    }

    virtual void hasBeenSimplified() const
    {
      cerr << "astinternal has been";
    }


    void SetNodeNum(int nn)
    {
      _node_num = nn;
    } //End of SetNodeNum()

  }; //End of Class ASTInternal
}; //end of namespace
#endif
