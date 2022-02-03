using System;
using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    [DebuggerDisplay("{DebugView}")]
    public class LinguisticState : IEquatable<LinguisticState>
    {
        public LinguisticState(string id, LinguisticStructureType type)
        {
            Id = id;
            Type = type;
        }

        public string Id { get; private set; }
        public LinguisticStructureType Type { get; private set; }

        private string DebugView => $"{Id}: {Type}";

        public bool Equals(LinguisticState other) => Id == other.Id;

        public override bool Equals(object obj) => obj is LinguisticState other && Equals(other);

        public override int GetHashCode() => Id.GetHashCode();

        public static bool operator ==(LinguisticState a, LinguisticState b) => a.Equals(b);
        public static bool operator !=(LinguisticState a, LinguisticState b) => !a.Equals(b);
    }
}
