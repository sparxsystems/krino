using Krino.Vertical.Utils.Enums;
using System;
using System.Linq;
using System.Reflection;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    internal static class EnumBaseExt
    {
        public static string GetGrammarId(this EnumBase source)
        {
            var result = new StringBuilder();

            object parentInstance = null;
            var path = source.ParentEnums.Reverse();
            foreach (var item in path)
            {
                if (item != path.First())
                {
                    // Property chain from the root.
                    var referencingProperty = GetReferencingProperty(parentInstance, item);
                    result.Append(referencingProperty.Name).Append(".");
                }

                parentInstance = item;
            }

            // If this is not the root.
            if (parentInstance != null)
            {
                var property = GetReferencingProperty(parentInstance, source);
                result.Append(property.Name);
            }

            return result.ToString();
        }

        private static PropertyInfo GetReferencingProperty(object instance, object value)
        {
            var properties = instance.GetType().GetProperties();
            var referencingProperty = properties.First(x => x.GetValue(instance) == value);
            return referencingProperty;
        }
    }
}
