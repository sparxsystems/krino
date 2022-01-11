using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    [DebuggerDisplay("{DebugView}")]
    public class LinguisticState
    {
        public LinguisticState(string id, LinguisticStructureType type)
        {
            Id = id;
            Type = type;
        }

        public string Id { get; private set; }
        public LinguisticStructureType Type { get; private set; }

        private string DebugView => $"{Id}: {Type}";
    }
}
