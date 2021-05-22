using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface ILinguisticStructureFactory
    {
        IWord CreateWord(IAdTree wordAdTree);
        ITerm CreateTerm(IAdTree termAdTree);
        IClause CreateClause(IAdTree clauseAdTree);
        ISentence CreateSentence(IAdTree sentenceAdTree);
    }
}
