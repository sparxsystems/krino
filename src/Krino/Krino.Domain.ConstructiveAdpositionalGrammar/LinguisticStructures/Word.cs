using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Word : LinguisticStructureBase, IWord
    {
        public Word(IAdTree wordAdTree, IAttributesModel attributesModel, ILinguisticStructureFactory factory, BigInteger attributes)
            : base(wordAdTree, attributesModel, factory, attributes)
        {
        }
    }
}
