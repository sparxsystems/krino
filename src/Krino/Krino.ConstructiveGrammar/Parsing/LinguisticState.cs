using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.ConstructiveGrammar.Parsing
{
    [DebuggerDisplay("{Id}")]
    public class LinguisticState : IEquatable<LinguisticState>
    {
        public LinguisticState(string id, BigInteger attributes)
        {
            Id = id;
            Attributes = attributes;
        }

        public string Id { get; private set; }
        public BigInteger Attributes { get; private set; }

        public string AttributesStr => GrammarAttributes.Instance.GetFullName(Attributes);


        // Note: this is to provide a readable info for DebuggerDisplay in StateDefinition<TState>.
        public override string ToString() => $"{Id}";



        public bool Equals(LinguisticState other) => other != null && Id == other.Id;

        public override bool Equals(object obj) => obj is LinguisticState other && Equals(other);

        public override int GetHashCode() => Id.GetHashCode();
    }
}
