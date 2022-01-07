using System.Collections.Generic;
using System.Numerics;
using System.Text;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Abstract class to declare the root of the structured enum.
    /// </summary>
    public abstract class EnumRootBase : EnumGroupBase
    {
        /// <summary>
        /// Constructs the root of the structured enum.
        /// </summary>
        protected EnumRootBase() : base(null) { }

        /// <summary>
        /// Number of bits the whole enum occupies.
        /// </summary>
        new public int Length { get; internal set; }

        /// <summary>
        /// Returns value for the value for the structured enum name.
        /// </summary>
        /// <param name="structuredEnumName"></param>
        /// <returns></returns>
        public BigInteger GetValue(string structuredEnumName)
        {
            var result = GetValueInternal("", 1, structuredEnumName);
            return result;
        }

        protected BigInteger GetValueInternal(string path, int pathIdx, string itemNameToResolve)
        {
            BigInteger result = 0;

            var newPath = new StringBuilder(path);

            // If it is a group.
            if (itemNameToResolve.StartsWith('('))
            {
                var splitGroup = SplitName(itemNameToResolve);
                if (splitGroup.Count > 1)
                {
                    foreach (var groupItem in splitGroup)
                    {
                        var subValue = GetValueInternal(newPath.ToString(), pathIdx, groupItem);
                        result |= subValue;
                    }
                }
                else
                {
                    if (newPath.Length > 0)
                    {
                        newPath.Append(".");
                    }
                    newPath.Append(splitGroup[0]);

                    var newPathSplit = newPath.ToString().Split(".");
                    var subValue = GetValueFromPath(newPathSplit, pathIdx);
                    result |= subValue;
                }
            }
            // Splitting the path.
            else
            {
                var splitPath = SplitName(itemNameToResolve);
                for (int i = 0; i < splitPath.Count; ++i)
                {
                    var pathItem = splitPath[i];

                    // If it is a group on the path.
                    // Note: group can be located only at the very end of the path.
                    if (pathItem.StartsWith('('))
                    {
                        var subValue = GetValueInternal(newPath.ToString(), pathIdx, pathItem);
                        result |= subValue;
                    }
                    // It is a pure path item.
                    else
                    {
                        if (newPath.Length > 0)
                        {
                            newPath.Append(".");
                        }
                        newPath.Append(pathItem);

                        // Note: If the condition is commented then it will end with the most possible value.
                        if (i + 1 == splitPath.Count)
                        {
                            var newPathSplit = newPath.ToString().Split(".");
                            var subValue = GetValueFromPath(newPathSplit, pathIdx);
                            result |= subValue;
                        }
                    }
                }
            }


            return result;
        }

        private List<string> SplitName(string name)
        {
            var result = new List<string>();

            bool isGroup = name.StartsWith('(');

            int bracketCounter = 0;
            int i = isGroup ? 1 : 0;
            int startIdx = i;
            int length = isGroup ? name.Length - 1 : name.Length;
            for (; i < length; ++i)
            {
                var c = name[i];

                switch (c)
                {
                    case '.':
                    case ',':
                        {
                            if (bracketCounter == 0 &&
                                (isGroup && c == ',' || !isGroup && c == '.'))
                            {
                                if (startIdx < i)
                                {
                                    var item = name.Substring(startIdx, i - startIdx);
                                    result.Add(item);
                                }
                                startIdx = i + 1;
                            }
                            break;
                        }
                    case '(':
                        {
                            ++bracketCounter;
                            break;
                        }
                    case ')':
                        {
                            --bracketCounter;

                            if (bracketCounter == 0)
                            {
                                if (startIdx < i)
                                {
                                    // +1 is to include the enclosing bracket.
                                    var item = name.Substring(startIdx, i - startIdx + 1);
                                    result.Add(item);
                                }
                                startIdx = i + 1;
                            }
                            break;
                        }
                    default:
                        {
                            // If this is the last character and there is an item to be added.
                            if (i + 1 == length && startIdx < length)
                            {
                                var item = name.Substring(startIdx, length - startIdx);
                                result.Add(item);
                            }

                            break;
                        }
                }
            }

            return result;
        }
    }
}
