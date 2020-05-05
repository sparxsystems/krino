using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.SemanticAttributesArrangement
{
    /// <summary>
    /// Aspect attributes.
    /// </summary>
    public class AspectAttributes : EnumGroupBase
    {
        public AspectAttributes(EnumGroupBase parent) : base(parent)
        {
            Simple = new EnumValue(this);
            Continous = new EnumValue(this);
            Perfect = new EnumValue(this);
        }

        public EnumValue Simple { get; }
        public EnumValue Continous { get; }
        public EnumValue Perfect { get; }
    }
}
