using System.Numerics;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public class Suppletion
    {
        public Suppletion(string value, BigInteger attributes = default)
        {
            Value = value;
            Attributes = attributes;
        }
        public string Value { get; private set; }
        public BigInteger Attributes { get; private set; }
    }
}
