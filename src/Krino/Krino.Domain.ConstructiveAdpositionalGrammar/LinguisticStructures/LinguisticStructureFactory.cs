using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public class LinguisticStructureFactory : ILinguisticStructureFactory
    {
        private IAttributesModel myAttributesModel;

        public LinguisticStructureFactory(IAttributesModel attributesModel)
        {
            myAttributesModel = attributesModel;
        }

        public IWord CreateWord(IAdTree wordAdTree, BigInteger attributes) => new Word(wordAdTree, myAttributesModel, this, attributes);


        public ITerm CreateTerm(IAdTree termAdTree, BigInteger attributes) => new Term(termAdTree, myAttributesModel, this, attributes);


        public IClause CreateClause(IAdTree clauseAdTree, BigInteger attributes) => new Clause(clauseAdTree, myAttributesModel, this, attributes);


        public ISentence CreateSentence(IAdTree sentenceAdTree) => new Sentence(sentenceAdTree, myAttributesModel, this);
    }
}
