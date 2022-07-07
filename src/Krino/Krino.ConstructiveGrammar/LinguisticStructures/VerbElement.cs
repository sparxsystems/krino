using System.Numerics;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    internal class VerbElement : VerbElementBase, IVerbElement
    {
        public VerbElement(BigInteger attributes) : base(attributes)
        {
        }

        protected override PhraseBase FactoryMethod() => new VerbElement(Attributes);
    }
}
