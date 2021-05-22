using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public class LinguisticStructureFactory : ILinguisticStructureFactory
    {
        private IAttributesModel myAttributesModel;

        public LinguisticStructureFactory(IAttributesModel attributesModel)
        {
            myAttributesModel = attributesModel;
        }

        public IWord CreateWord(IAdTree wordAdTree) => new Word(wordAdTree);


        public ITerm CreateTerm(IAdTree termAdTree) => new Term(termAdTree, myAttributesModel, this);


        public IClause CreateClause(IAdTree clauseAdTree) => new Clause(clauseAdTree, myAttributesModel, this);
        

        public ISentence CreateSentence(IAdTree sentenceAdTree)
        {
            throw new NotImplementedException();
        }

        
    }
}
