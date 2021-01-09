using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Attributing.Semantic
{
    /// <summary>
    /// Person attributes.
    /// </summary>
    public class PersonSememes : EnumGroupBase
    {
        public PersonSememes(EnumGroupBase parent) : base(parent)
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
