using Krino.Domain.ConstructiveGrammar.Parsing;
using System.Numerics;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures
{
    public abstract class LinguisticStructureBase
    {
        public LinguisticStructureBase(BigInteger attributes)
        {
            Attributes = attributes;
        }

        public virtual BigInteger Attributes { get; protected set; }

        public string AttributesStr => Attributes.GetGrammarId();
    }
}
