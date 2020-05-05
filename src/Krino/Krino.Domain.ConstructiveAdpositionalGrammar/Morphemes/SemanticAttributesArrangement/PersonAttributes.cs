using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.SemanticAttributesArrangement
{
    /// <summary>
    /// Person attributes.
    /// </summary>
    public class PersonAttributes : EnumGroupBase
    {
        public PersonAttributes(EnumGroupBase parent) : base(parent)
        {
            First = new EnumValue(this);
            Second = new EnumValue(this);
            Third = new EnumValue(this);
        }

        public EnumValue First { get; }

        public EnumValue Second { get; }

        public EnumValue Third { get; }
    }
}
