import { Box, Flex, Text, VStack } from '@devup-ui/react'

import { SideMenu } from '@/components/side-menu'
import { SideMenuProvider } from '@/components/side-menu/side-menu-provider'
import { SIDE_MENU_ITEMS } from '@/constants'

export default function Page() {
  return (
    <Flex maxW="1440px" mx="auto" pt="68px" w="100%">
      <Box
        borderRight="solid 1px $border"
        display={['none', null, null, 'block']}
      >
        <SideMenuProvider>
          <VStack
            alignItems="center"
            gap="$spacingSpacing04"
            pos="sticky"
            px="$spacingSpacing16"
            py="28px"
            top="68px"
            w="250px"
          >
            <VStack alignItems="center" gap="12px" w="100%">
              <VStack w="100%">
                {SIDE_MENU_ITEMS.documentation.map(
                  ({ value, label, children }) => (
                    <SideMenu key={value} childMenus={children} value={value}>
                      {label}
                    </SideMenu>
                  ),
                )}
              </VStack>
            </VStack>
          </VStack>
        </SideMenuProvider>
      </Box>
      <VStack
        flex="1"
        gap="16px"
        minH="calc(100dvh - 280px)"
        overflow="hidden"
        pb={['60px', null, null, '$spacingSpacing32']}
        pt={['$spacingSpacing12', null, null, '$spacingSpacing32']}
        px={['$spacingSpacing20', null, null, '$spacingSpacing48']}
      >
        <Text color="$caption" typography="captionB">
          Overview
        </Text>
        <VStack gap="$spacingSpacing40">
          <VStack gap="16px">
            <Text color="$title" typography="h2">
              Why Vespera?
            </Text>
            <Text color="$text" typography="body">
              Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam
              venenatis, elit in hendrerit porta, augue ante scelerisque diam,
              ac egestas lacus est nec urna. Cras commodo risus hendrerit,
              suscipit nibh at, porttitor dui. Vivamus tincidunt pretium nibh et
              pulvinar. Nam quis tristique neque, vitae facilisis justo. Ut non
              tristique dui. Suspendisse aliquam nunc ligula, vitae elementum
              leo luctus nec. Nunc eu mi ut risus hendrerit accumsan. Duis
              ultrices arcu sed sapien pellentesque ultricies. Mauris vel
              convallis sem, non ullamcorper tortor.
              <br />
              <br />
              Aliquam volutpat mi quis augue mattis fringilla. Nullam at metus
              consectetur, ornare dolor vel, convallis nisl. Morbi in mi risus.
              Ut sed metus nisi. Praesent accumsan vitae leo a mattis. Morbi id
              maximus orci. Cras eu varius tellus. Aenean rutrum dictum quam eu
              ornare. Integer porttitor pretium tellus, et congue ligula
              hendrerit nec. Etiam interdum et nunc sit amet aliquam. Nullam vel
              ultrices neque.
              <br />
              <br />
              Praesent at metus eget mauris gravida blandit. Morbi id dictum
              lacus, et varius orci. Maecenas rutrum imperdiet arcu, vitae
              imperdiet justo luctus sollicitudin. Quisque at lacinia mi.
              Suspendisse potenti. Aliquam eu magna vel sem commodo pretium sed
              eu est. Integer porttitor, lacus at sagittis volutpat, ligula nisi
              accumsan felis, a hendrerit arcu eros in dui. Etiam sagittis,
              nulla eget varius fermentum, lectus massa rutrum metus, ac rutrum
              dui tellus at massa.
              <br />
              <br />
              Cras euismod tempor varius. Aliquam quis tempus nisl, quis
              ultricies dui. Aliquam quam nisl, porta hendrerit sodales et,
              ornare sed ipsum. Vestibulum sed iaculis turpis. Curabitur
              volutpat interdum nulla, blandit dignissim libero facilisis sed.
              Donec eu arcu ex. Nulla sit amet lectus sit amet diam luctus
              tempor a ut odio. Donec aliquet suscipit egestas. Pellentesque
              habitant morbi tristique senectus et netus et malesuada fames ac
              turpis egestas. Vivamus vitae faucibus eros.{'  '}
            </Text>
          </VStack>
          <VStack gap="16px">
            <Text color="$title" typography="h2">
              Key Advantages
            </Text>
            <Text color="$text" typography="body">
              Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam
              venenatis, elit in hendrerit porta, augue ante scelerisque diam,
              ac egestas lacus est nec urna. Cras commodo risus hendrerit,
              suscipit nibh at, porttitor dui. Vivamus tincidunt pretium nibh et
              pulvinar. Nam quis tristique neque, vitae facilisis justo. Ut non
              tristique dui. Suspendisse aliquam nunc ligula, vitae elementum
              leo luctus nec. Nunc eu mi ut risus hendrerit accumsan. Duis
              ultrices arcu sed sapien pellentesque ultricies. Mauris vel
              convallis sem, non ullamcorper tortor.
              <br />
              <br />
              Aliquam volutpat mi quis augue mattis fringilla. Nullam at metus
              consectetur, ornare dolor vel, convallis nisl. Morbi in mi risus.
              Ut sed metus nisi. Praesent accumsan vitae leo a mattis. Morbi id
              maximus orci. Cras eu varius tellus. Aenean rutrum dictum quam eu
              ornare. Integer porttitor pretium tellus, et congue ligula
              hendrerit nec. Etiam interdum et nunc sit amet aliquam. Nullam vel
              ultrices neque.{'  '}
            </Text>
          </VStack>
          <VStack gap="16px">
            <Text color="$title" typography="h2">
              Proven Performance
            </Text>
            <Text color="$text" typography="body">
              Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam
              venenatis, elit in hendrerit porta, augue ante scelerisque diam,
              ac egestas lacus est nec urna. Cras commodo risus hendrerit,
              suscipit nibh at, porttitor dui. Vivamus tincidunt pretium nibh et
              pulvinar. Nam quis tristique neque, vitae facilisis justo. Ut non
              tristique dui. Suspendisse aliquam nunc ligula, vitae elementum
              leo luctus nec. Nunc eu mi ut risus hendrerit accumsan. Duis
              ultrices arcu sed sapien pellentesque ultricies. Mauris vel
              convallis sem, non ullamcorper tortor.
              <br />
              <br />
              Aliquam volutpat mi quis augue mattis fringilla. Nullam at metus
              consectetur, ornare dolor vel, convallis nisl. Morbi in mi risus.
              Ut sed metus nisi. Praesent accumsan vitae leo a mattis. Morbi id
              maximus orci. Cras eu varius tellus. Aenean rutrum dictum quam eu
              ornare. Integer porttitor pretium tellus, et congue ligula
              hendrerit nec. Etiam interdum et nunc sit amet aliquam. Nullam vel
              ultrices neque.{'  '}
            </Text>
          </VStack>
        </VStack>
      </VStack>
      <Box display={['none', null, null, 'block']}>
        <VStack
          gap="16px"
          overflow="hidden"
          pos="sticky"
          px="$spacingSpacing16"
          py="28px"
          top="68px"
          w="180px"
        >
          <VStack borderBottom="solid 1px $border" pb="$spacingSpacing08">
            <Flex alignItems="center" py="6px">
              <Text color="$text" flex="1" typography="captionB">
                Contents
              </Text>
              {/* <MenuIcon Property1="bottom" /> */}
            </Flex>
            <VStack>
              {/* <Anchor property1="selected" />
              <Anchor property1="default" />
              <Anchor property1="default" /> */}
            </VStack>
          </VStack>
          {/* <Edit property1="default" /> */}
        </VStack>
      </Box>
    </Flex>
  )
}
