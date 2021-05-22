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
        private ILinguisticStructureFactory myFactory;

        public Term(IAdTree adTree, IAttributesModel attributesModel, ILinguisticStructureFactory factory)
        {
            AdTree = adTree;
            myAttributesModel = attributesModel;
            myFactory = factory;
        }

        public IAdTree AdTree { get; private set; }

        public BigInteger StructureAttributes => 0;

        public string Value => AdTree.Phrase;

        public IEnumerable<IWord> Words => AdTree.Where(x => myAttributesModel.IsLexeme(x.Morpheme.Attributes))
            .Select(x => myFactory.CreateWord(x));
    }
}
