using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Term : ITerm
    {
        private IAttributesModel myAttributesModel;
        private IWordFactory myWordFactory;

        public Term(IAdTree adTree, IAttributesModel attributesModel, IWordFactory wordFactory)
        {
            AdTree = adTree;
            myAttributesModel = attributesModel;
            myWordFactory = wordFactory;
        }

        public IAdTree AdTree { get; private set; }

        public BigInteger StructureAttributes => 0;

        public IEnumerable<IWord> Words => AdTree.Where(x => myAttributesModel.IsLexeme(x.Morpheme.Attributes))
            .Select(x => myWordFactory.Create(x));
    }
}
