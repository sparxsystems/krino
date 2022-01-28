using Krino.Vertical.Utils.Enums;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Text : LinguisticStructureBase, IText
    {
        public Text(EnumRootBase enumRoot)
            : base(enumRoot, 0)
        {
        }

        public List<ISentence> Sentences { get; } = new List<ISentence>();

        public string Value => string.Join(" ", Sentences.Select(x => x.Value));
    }
}
