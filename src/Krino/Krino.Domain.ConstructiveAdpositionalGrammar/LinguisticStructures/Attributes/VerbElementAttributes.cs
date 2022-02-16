using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes.Sememes;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    public class VerbElementAttributes : EnumGroupBase
    {
        public VerbElementAttributes(EnumGroupBase parent) : base(parent)
        {
            Sememe = new VerbSememes(this);
        }

        /// <summary>
        /// Semantic attributes.
        /// </summary>
        public VerbSememes Sememe { get; }
    }
}
