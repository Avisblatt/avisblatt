tea_include <- function(){
  #IDs added by Anna Reimann, ORCID 0000-0001-8225-7851
  c("56eeaaac-af5e-54d7-9c00-185229da55d5/t1", # Thee-Es. sentz (ocr-mistake, herbal)
    "57a30888-b244-5500-83eb-b7e1d46c39c9/t6", # Thee- (ocr-mistake)
    "e9f8930e-b27d-5062-9de8-add5db11dcf4/t2", # Problem with regex sometimes if written "Thee" 
    "857fad1b-6cb2-5710-a6f8-2266ac13ca29/t2", # Problem with regex sometimes if written "Thee" 
    "093c33e9-96ba-5342-8227-f9f6736dc1cc/a0", # Problem with regex sometimes if written "Thee" 
    "e30d8df0-f2cb-5b8d-9e7c-7a82c17347bc/t17", # Problem with regex sometimes if written "Thee" 
    "9feb90b1-a2d0-5b07-81db-e6094d76797b/t2", # Problem with regex sometimes if written "Thee" 
    "27edc68c-4241-5f71-99f8-b09625ab89ed/t1", # Problem with regex sometimes if written "Thee" 
    "2543ad51-cd45-5a89-8bc3-d5646221b083/t3", # Problem with regex sometimes if written "Thee" 
    "46d636d7-f5e6-5e90-84b3-7a9b6ac40ea9/t1", # Problem with regex sometimes if written "Thee" 
    "b0ff9151-586a-5afc-9ae2-05446c7d57e9/t17", # Problem with regex sometimes if written "Thee" 
    "2b28940c-b92a-5377-8ad0-3c1763466ec9/t9", # Problem with regex sometimes if written "Thee" 
    "4db7e647-b952-58dd-9f74-4b1f3d9de928/t8", # Problem with regex sometimes if written "Thee" 
    "abf9c87a-d9c4-59dd-8c9b-21a8cd0c58a9/t3", # Problem with regex sometimes if written "Thee" 
    "8e956b08-197b-5eca-a844-fb4f15e7502a/t28", # Problem with regex sometimes if written "Thee" 
    "839233bd-c3bb-5f07-9564-93eccf54fd0e/t28", # Problem with regex sometimes if written "Thee" 
    "cf646ba6-bc26-5bdd-b47f-cd4d30d4cc34/t16", # Problem with regex sometimes if written "Thee" 
    "73373e63-21e3-5581-9fec-8c74d5562243/t1", # Problem with regex sometimes if written "Thee" 
    "67526d53-c51c-526c-ab91-fde118e8b9cb/t7", # Problem with regex sometimes if written "Thee" 
    "9cc049ba-298a-50d1-b0b2-c56e7edac8b1/t1", # Problem with regex sometimes if written "Thee" 
    "5e8a64cd-4195-5cea-b9c6-c7b4c1efff7c/t11", # Problem with regex sometimes if written "Thee" 
    "9b7adabc-f83a-52b0-9536-f3db28eb0749/t5", # Problem with regex sometimes if written "Thee" 
    "8e22065e-17b7-5603-9709-649bebf0d09d/t11", # Problem with regex sometimes if written "Thee" 
    "15178327-1687-5384-9a4a-1cd04797d81e/t9", # Problem with regex sometimes if written "Thee" 
    "7436c83d-b954-5f88-883a-6b86d6db6705/t1", # Problem with regex sometimes if written "Thee" 
    "383a1242-2e8f-5740-9f9e-fa09067d0bca/t9", # Problem with regex sometimes if written "Thee" 
    "0a1ce6d8-59ab-5fe5-93a4-e67c438f54a5/t10", # Problem with regex sometimes if written "Thee" 
    "6aa59d72-d97f-54c8-a7ba-59a432d7a936/t2", # Problem with regex sometimes if written "Thee" 
    "3364be6d-c58e-5c5f-8248-6f785ceafcda/t6", # Problem with regex sometimes if written "Thee" 
    "bd75ed58-c1a7-55ec-a002-21f30afef70c/t17", # Problem with regex sometimes if written "Thee" 
    "026eb095-ac68-5f03-a40e-7438ca03c62f/t7", # Problem with regex sometimes if written "Thee" 
    "a7daf9ef-2837-5105-8a39-b62ea757849e/t18", # Problem with regex sometimes if written "Thee" 
    "d196f54c-cc30-5dcc-aeb1-cc3d08b76af2/t3", # Problem with regex sometimes if written "Thee" 
    "temp-1842-733-001", # Problem with regex sometimes if written "Thee" 
    "374c4535-09c6-585a-ae3d-988066896c01/t1", # Problem with regex sometimes if written "Thee" 
    "853d9206-6918-5b0e-8cf4-3e8063924103/t14", # Problem with regex sometimes if written "Thee" 
    "f37b9ec6-64d3-5e88-8fcf-6e04914a4a74/t9", # unclear why not included
    "8771cf78-e47a-5639-928d-39972e3ecf5a/t7", # unclear why not included
    "616b230b-1754-5142-8dfb-2b8e468d98ed/t8", # unclear why not included
    "ff449f8f-50e4-5fe3-8542-e32bc0501603/t1", # French (lower case thee)
    "1e08d433-e6a6-5ed6-80fd-d47083706404/t1", # Tea and bookstuff
    "ae058e69-89ff-552e-82ee-05aee3eac66b/t11", # Tea and bookstuff
    "c51f876b-8c7a-5ec9-bcf3-8dca2797a999/t31", # Tea and bookstuff
    "d5c41e50-244d-5e87-99ba-dc2344a3a46f/t13", # Tea and bookstuff
    "e81a8e04-424b-5c48-b1bc-3074c1c3a722/t23", # Tea and bookstuff
    "e1d114b3-fd69-5cf1-9ec5-cace27aadf6d/t21", # Tea and bookstuff
    "f52b9bcc-43c8-5ba9-9360-cd064346334e/t3", # Tea and bookstuff
    "ad240a1b-fb76-5bb5-a22f-d583b39ea235/t35", # Tea and bookstuff
    "e0de3b1f-311c-5dba-b9be-f35dab52dfb3/t1",  # Tea and bookstuff
    "e1d114b3-fd69-5cf1-9ec5-cace27aadf6d/t21", # Tea and bookstuff
    "f52b9bcc-43c8-5ba9-9360-cd064346334e/t3", # Tea and bookstuff
    "ad240a1b-fb76-5bb5-a22f-d583b39ea235/t35", # Tea and bookstuff
    "0b8a3296-8845-5a49-a624-63ad2f8033f8/t10", # Tea and bookstuff
    "550cceb5-074c-54ba-a10d-982bb3ac1471/t14", # Tea and bookstuff
    "2c56f1f6-d183-54e6-b65d-6ed5b02c8cb7/t14", # Tea and bookstuff
    "78bd7f32-06af-536b-b557-840ef3e81df1/t9", # Tea and bookstuff
    "00900c65-9247-54c7-9e1b-1cf8304d0a98/t3", # Tea and bookstuff
    "723f0065-0fa2-5803-927e-4d9866cd3a6b/t12", # Tea and bookstuff
    "bd75d0e4-cd96-549c-98e0-56ee243a43dd/t5", # Tea and bookstuff
    "3a4414f4-e335-5711-a074-816fade6be3d/t5", # Tea and bookstuff
    "4928e655-fc7f-59f0-bac7-391f673b82bd/t14", # Tea and bookstuff
    "2b510de1-fa12-57c4-aa2a-07a60dd87f02/t16", # Tea and bookstuff
    "7b244679-12aa-50d8-82fe-f441b0276e16/t9", # Tea and bookstuff
    "fd834625-25e1-5e1e-94ad-5cbadbc7d95a/t13", # Tea and bookstuff
    "feed9e8e-1b71-5c72-85d7-14f5cc9f552c/t8", # Tea and bookstuff
    "04e9bf31-5a4d-5242-ba51-8ee5bbe3fe54/a7", # Tea and bookstuff
    "d78c476f-e6a6-5972-ae49-ab1308ea62a5/a1", # Tea and bookstuff
    "026eb095-ac68-5f03-a40e-7438ca03c62f/t7", # Tea and bookstuff
    "f4e745da-c838-57e1-bc90-b22b82a314a6/t0", # Tea and bookstuff
    "fa52c30c-4fcd-53e6-9016-879dbfa6fedc/t1", # Tea and bookstuff
    "5149faa2-154d-53d7-ae9b-d5ebe21fb176/t9", # Tea and bookstuff
    "2ab3b5bc-6b82-55d8-a0e8-e4a31ee1efa4/t29", # Tea and bookstuff
    "temp-1814-115-005", # Tea and bookstuff
    "6a1bf8a4-2081-5064-880f-0ad280199c41/a13", # Tea and bookstuff
    "4edfd96f-ea89-52cb-8929-bc3eb1908f30/t2", # unclear why excluded
    "64b616a3-aa58-5072-b7a2-6fa0bc1975b1/t7", # unclear why excluded
    "89a30b6e-d9b5-533b-a2b5-0d619bdb1ac8/t2", # unclear why excluded
    "1d5fa125-effd-5ad2-bf7a-1272b4f88ce2/a8", # unclear why excluded
    "b6fe4597-d0a4-58ff-a23e-701f015eccd2/t11", # unclear why excluded
    "d7d4ef70-2e80-5c77-befa-9958385aee12/t6", # unclear why excluded
    "e887dc42-5e2e-5c1c-a0b9-49bf49a64a69/t4", # unclear why excluded
    "ba56f8d5-2b2a-513d-b230-f9dba4b9f6ec/t8", # unclear why excluded
    "2ae09245-4710-56a2-bfee-0d6cf4d997f9/t4", # unclear why excluded
    "7e8a6e69-bc1a-52b7-840e-92224964614f/t7", # ocr mistake
    "0d9bc887-f9a3-59c4-895d-876332b95591/t4", # ocr mistake
    "badc3509-0129-54a7-9782-57ca2fb0b0ab/t10", # ocr mistake
    "3558e0e1-1d3d-50d9-b4c8-0d84e3b5d339/t1", # ocr mistake
    "d7873185-f697-5d21-b81f-3a5e52b5c2a5/t13", # ocr mistake
    "755c2e0e-e459-5495-bf4e-69df7b6c7b2f/t16", # Tea and bookstuff
    "e8c50c3b-67fa-54e4-85b6-01851760394b/t17", # Tea and bookstuff
    "0dc2b985-a741-511d-b445-9baa6fe83734/t23" # Tea and bookstuff
  )
}