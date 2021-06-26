using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public abstract class LinguisticStructureBase
    {
        public LinguisticStructureBase(IAdTree adTree, IAttributesModel attributesModel, ILinguisticStructureFactory factory, BigInteger attributes)
        {
            AdTree = adTree;
            AttributesModel = attributesModel;
            Factory = factory;
            Attributes = attributes;
        }


        public IAdTree AdTree { get; private set; }

        public virtual BigInteger Attributes { get; protected set; }

        public string Value => AdTree.Phrase;

        protected IAttributesModel AttributesModel { get; private set; }

        protected ILinguisticStructureFactory Factory { get; private set; }
    }
}
