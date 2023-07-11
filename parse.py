def parseTypeSignature(signatre:str):
    keywords = ["!","?","End"]
    separator = "."
    type_store = []
    # Remove all spaces
    signatre = signatre.replace(" ","")
    # Split the string into a list of strings
    signatre = signatre.split(separator)
    # Check if the last element is a keyword
    if signatre[-1] != "End":
        raise Exception("Invalid type signature, Ending should be 'End'")
    else:
        # Remove the last element
        signatre = signatre[:-1]
    # For each element in the list, check if the last character is a keyword
    for i in range(len(signatre)):
        if signatre[i][-1] in keywords:
            # If it is, remove the last character
            # signatre[i] = signatre[i][:-1]
            # type_store.append([signatre[i][:-1], signatre[i][-1]])
            type_store.append({"type":signatre[i][:-1], "keyword":signatre[i][-1]})
        else:
            # If it is not, raise an exception
            raise Exception("Invalid type signature, missing keyword for element " + signatre[i])
        
    return type_store

test = "A!.B?.C!.End"
print(parseTypeSignature(test))

def gen_counter_type_series(original_type_series):
    counter_type_series = []
    for i in range(len(original_type_series)):
        if original_type_series[i]["keyword"] == "!":
            counter_type_series.append({"type":original_type_series[i]["type"], "keyword":"?"})
        elif original_type_series[i]["keyword"] == "?":
            counter_type_series.append({"type":original_type_series[i]["type"], "keyword":"!"})
        else:
            raise Exception("Invalid type signature, missing keyword for element " + original_type_series[i]["type"])
    return counter_type_series

print(gen_counter_type_series(parseTypeSignature(test)))