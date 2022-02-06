using Krino.Vertical.Utils.Enums;
using System;
using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    [DebuggerDisplay("{Id}")]
    public class LinguisticState : IEquatable<LinguisticState>
    {
        public LinguisticState(string id, EnumBase type)
        {
            Id = id;
            Type = type;
        }

        public string Id { get; private set; }
        public EnumBase Type { get; private set; }

        // Note: this is overrident to provide readable info of DebuggerDisplay for StateDefinition<TState>.
        public override string ToString() => $"{Id}";



        public bool Equals(LinguisticState other) => other != null && Id == other.Id;

        public override bool Equals(object obj) => obj is LinguisticState other && Equals(other);

        public override int GetHashCode() => Id.GetHashCode();

        //public static bool operator ==(LinguisticState a, LinguisticState b) => a != null && a.Equals(b);
        //public static bool operator !=(LinguisticState a, LinguisticState b) => !(a == b);

        //public static bool operator ==(LinguisticState a, string b) => a.Id == b;
        //public static bool operator !=(LinguisticState a, string b) => a.Id != b;
    }
}
