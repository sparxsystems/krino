using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using System.Collections.Generic;
using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    [DebuggerDisplay("{DebuggerDisplay}")]
    internal class WordAdTrees
    {
        public WordAdTrees(string word)
        {
            Word = word;
        }

        public string Word { get; private set; }

        public List<IAdTree> AdTrees { get; private set; } = new List<IAdTree>();

        private string DebuggerDisplay => Word;
    }
}
