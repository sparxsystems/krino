using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Term : LinguisticStructureBase, ITerm
    {
        public Term(IAdTree termAdTree, IAttributesModel attributesModel, ILinguisticStructureFactory factory, BigInteger attributes)
            : base(termAdTree, attributesModel, factory, attributes)
        {
        }

        public IEnumerable<IWord> Words => AdTree.Where(x => AttributesModel.IsLexeme(x.Morpheme.Attributes))
            .Select(x => Factory.CreateWord(x.MakeDeepCopy(), 0));
    }
}
