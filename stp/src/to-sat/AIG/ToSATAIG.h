// -*- c++ -*-
/********************************************************************
 * AUTHORS: Trevor Hansen
 *
 * BEGIN DATE: June, 2010
 *
 * LICENSE: Please view LICENSE file in the home dir of this Program
 ********************************************************************/

#ifndef TOSATAIG_H
#define TOSATAIG_H
#include <cmath>

#include "../../AST/AST.h"
#include "../../AST/RunTimes.h"
#include "../../STPManager/STPManager.h"
#include "../BitBlaster.h"
#include "BBNodeManagerAIG.h"
#include "ToCNFAIG.h"

namespace BEEV
{

  class ToSATAIG : public ToSATBase
  {
  private:

    ASTNodeToSATVar nodeToSATVar;
    simplifier::constantBitP::ConstantBitPropagation* cb;

    // don't assign or copy construct.
    ToSATAIG&  operator = (const ToSATAIG& other);
    ToSATAIG(const ToSATAIG& other);

	int count;
	bool first;
	BitBlaster<BBNodeAIG, BBNodeManagerAIG> *bb;
	int CNFFileNameCounter;
	BBNodeManagerAIG mgr;
	Simplifier *simp;

    ToCNFAIG toCNF;

    void init()
    {
        count = 0;
        first = true;
        bb = NULL;
        CNFFileNameCounter =0;
        simp = NULL;
    }

  public:

    bool cbIsDestructed()
    {
    	return cb == NULL;
    }


    ToSATAIG(STPMgr * bm) :
      ToSATBase(bm), toCNF(bm->UserFlags)
    {
      cb = NULL;
      init();
    }

    ToSATAIG(STPMgr * bm, simplifier::constantBitP::ConstantBitPropagation* cb_) :
    	ToSATBase(bm), cb(cb_), toCNF(bm->UserFlags)
    {
      cb = cb_;
      init();
    }

    ~ToSATAIG();

    void
    ClearAllTables()
    {
      nodeToSATVar.clear();
    }

    // Used to read out the satisfiable answer.
    ASTNodeToSATVar&
    SATVar_to_SymbolIndexMap()
    {
      return nodeToSATVar;
    }

    bool  CallSAT(SATSolver& satSolver, const ASTNode& input, bool needAbsRef);

  };
}

#endif
