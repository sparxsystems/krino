using Krino.ConstructiveGrammar.LinguisticStructures.Attributes.Sememes;
using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class VerbPhraseAttributes : EnumGroupBase
    {
        public VerbPhraseAttributes(EnumGroupBase parent) : base(parent)
        {
            Sememe = new VerbSememes(this);
        }

        /// <summary>
        /// Semantic attributes.
        /// </summary>
        public VerbSememes Sememe { get; }
    }
}
