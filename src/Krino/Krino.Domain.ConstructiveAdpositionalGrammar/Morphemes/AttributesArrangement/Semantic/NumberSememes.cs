using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Semantic
{
    /// <summary>
    /// Tense attributes.
    /// </summary>
    public class NumberSememes : EnumGroupBase
    {
        public NumberSememes(EnumGroupBase parent) : base(parent)
        {
            Past = new EnumValue(this);
            Present = new EnumValue(this);
            Future = new EnumValue(this);
        }

        public EnumValue Past { get; }

        public EnumValue Present { get; }

        public EnumValue Future { get; }
    }
}
