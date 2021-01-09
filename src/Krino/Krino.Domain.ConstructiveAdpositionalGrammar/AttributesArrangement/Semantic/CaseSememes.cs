using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AttributesArrangement.Semantic
{
    /// <summary>
    /// Case attributes.
    /// </summary>
    public class CaseSememes : EnumGroupBase
    {
        public CaseSememes(EnumGroupBase parent) : base(parent)
        {
            Nominative = new EnumValue(this);
            Genitive = new EnumValue(this);
            Accusative = new EnumValue(this);
        }

        public EnumValue Nominative { get; }

        public EnumValue Genitive { get; }

        public EnumValue Accusative { get; }
    }
}
