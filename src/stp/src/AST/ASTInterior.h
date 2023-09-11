// -*- c++ -*-
/********************************************************************
 * AUTHORS: Vijay Ganesh
 *
 * BEGIN DATE: November, 2005
 *
 * LICENSE: Please view LICENSE file in the home dir of this Program
 ********************************************************************/

#ifndef ASTINTERIOR_H
#define ASTINTERIOR_H

#include <tr1/memory>
#include "UsefulDefs.h"
#include "ASTInternalWithChildren.h"

namespace BEEV
{
  class ASTNode;
  class STPMgr;
  typedef vector<ASTNode> ASTVec;
  
  /******************************************************************
   * Class ASTInterior:                                             *
   *                                                                *
   * Internal representation of an interior ASTNode.Generally, these*
   * nodes should have at least one child                           * 
   ******************************************************************/
  class ASTInterior: public ASTInternalWithChildren
  {

    friend class STPMgr;
    friend class ASTNodeHasher;
    friend class ASTNodeEqual;
    friend BEEV::ASTNode HashingNodeFactory::CreateNode(const Kind kind, const BEEV::ASTVec & back_children);

  private:
    /******************************************************************
     * Private Data and Member Functions                              *
     ******************************************************************/

    /******************************************************************
     * Class ASTInteriorHasher:                                       *
     *                                                                *
     * Hasher for ASTInterior pointer nodes                           *
     ******************************************************************/
    class ASTInteriorHasher
    {
    public:
      size_t operator()(const ASTInterior *int_node_ptr) const;
    }; //End of ASTInteriorHasher

    /******************************************************************
     * Class ASTInteriorEqual:                                        *
     *                                                                *
     * Equality for ASTInterior nodes                                 *
     ******************************************************************/
    class ASTInteriorEqual
    {
    public:
      bool operator()(const ASTInterior *int_node_ptr1, 
                      const ASTInterior *int_node_ptr2) const;
    }; //End of class ASTInteriorEqual

    // Used in Equality class for hash tables
    friend bool operator==(const ASTInterior &int_node1, 
                           const ASTInterior &int_node2)
    {
      return ((int_node1._kind == int_node2._kind) 
              && (int_node1._children == int_node2._children));
    } //End of operator==

    // Returns kinds.  "lispprinter" handles printing of parenthesis
    // and childnodes. (c_friendly is for printing hex. numbers that C
    // compilers will accept)
    virtual void nodeprint(ostream& os, bool c_friendly = false);

  public:
    /******************************************************************
     * Public Member Functions                                        *
     ******************************************************************/
    
    // Basic constructors
    ASTInterior(Kind kind, std::tr1::weak_ptr<STPMgr> bm) : ASTInternalWithChildren(kind, bm)
    {
    }

    ASTInterior(Kind kind, ASTVec &children, std::tr1::weak_ptr<STPMgr> bm) : ASTInternalWithChildren(kind, children, bm)
    {
    }

    //Copy constructor.  This copies the contents of the child nodes
    //array, along with everything else. Assigning the smart pointer,
    //ASTNode, does NOT invoke this.
    ASTInterior(const ASTInterior &int_node) : ASTInternalWithChildren(int_node)
    {
    }

    // Destructor (does nothing, but is declared virtual here.
    virtual ~ASTInterior()
    {
    }
  }; //End of ASTNodeInterior
}; //end of namespace BEEV
#endif
